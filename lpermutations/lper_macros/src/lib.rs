#[proc_macro_derive(PermutationEnum)]
pub fn permutation_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input as syn::DeriveInput);

    let enum_name = &input.ident;

    let variants = match &input.data {
        syn::Data::Enum(data) => &data.variants,
        _ => {
            return syn::Error::new_spanned(input, "expected an enum")
                .to_compile_error()
                .into();
        }
    };

    let mut prev_count = quote::quote! { 0usize };
    let mut variant_counts = Vec::new();
    let mut froms = Vec::new();
    let mut intos = Vec::new();

    for variant in variants {
        let variant_name = &variant.ident;
        let count = match &variant.fields {
            syn::Fields::Unit => quote::quote! { 1usize },
            syn::Fields::Unnamed(fields) => {
                let types = fields.unnamed.iter().map(|field| &field.ty);
                quote::quote! {1usize #(* #types::total_permutations())*}
            }
            syn::Fields::Named(fields) => {
                let types = fields.named.iter().map(|field| &field.ty);
                quote::quote! {1usize #(* #types::total_permutations())*}
            }
        };
        let from = match &variant.fields {
            syn::Fields::Unit => quote::quote! { 
                if n == #prev_count {
                    return #enum_name::#variant_name;
                }
            },
            syn::Fields::Unnamed(fields) => {
                let types: Vec<_> = fields.unnamed.iter().map(|f| &f.ty).collect();
                let mut field_offsets = Vec::new();
                let mut offset = quote::quote! { 0usize };
                for ty in &types {
                    field_offsets.push(offset.clone());
                    offset = quote::quote! {
                        (#offset + #ty::total_permutations())
                    };
                }
                let checks = types.iter().zip(field_offsets.iter()).map(|(ty, offset)| {
                    quote::quote! {
                        if n < (#prev_count + #offset + #ty::total_permutations()) {
                            return Some(
                                #enum_name::#variant_name(
                                    #ty::from_index(n - (#prev_count + #offset))?
                                )
                            );
                        }
                    }
                });
                quote::quote! {
                    if n >= #prev_count && n < (#prev_count + #count) {
                        #(#checks)*
                    }
                }
            }
            syn::Fields::Named(fields) => {
                let types: Vec<_> = fields.named.iter().map(|f| &f.ty).collect();
                let mut field_offsets = Vec::new();
                let mut offset = quote::quote! { 0usize };
                for ty in &types {
                    field_offsets.push(offset.clone());
                    offset = quote::quote! {
                        (#offset + #ty::total_permutations())
                    };
                }
                let checks = types.iter().zip(field_offsets.iter()).map(|(ty, offset)| {
                    quote::quote! {
                        if n < (#prev_count + #offset + #ty::total_permutations()) {
                            return Some(
                                #enum_name::#variant_name(
                                    #ty::from_index(n - (#prev_count + #offset))?
                                )
                            );
                        }
                    }
                });
                quote::quote! {
                    if n >= #prev_count && n < (#prev_count + #count) {
                        #(#checks)*
                    }
                }
            }
        };
        froms.push(from);
        intos.push(
            quote::quote! {
                #enum_name::#variant_name(a) => {
                    return Some(#prev_count + #count + a.current_permutation());
                }
            }
        );
        prev_count = quote::quote! { (#prev_count) + (#count) };
        variant_counts.push(count);
    }

    let total = quote::quote! {
        0usize #(+ #variant_counts)*
    };

    quote::quote! {
        impl crate::lpermutations::Permutation for #enum_name {
            fn total_permutations() -> usize {
                #total
            }
            fn current_permutation(&self) -> Option<usize> { 
                match self {
                    #(#intos)*
                    _ => {
                        return None;
                    }
                }
            }
            fn from_index(n: usize) -> Option<Self> {
                #(#froms)*
                return None;
            }
        }
    }.into()
}
fn eval_expr(expr: &syn::Expr) -> syn::Result<i64> {
    use syn::{Expr, BinOp, UnOp};
    match expr {
        Expr::Lit(expr) => {
            if let syn::Lit::Int(lit) = &expr.lit {
                lit.base10_parse::<i64>()
            } else {
                Err(syn::Error::new_spanned(expr, "Expected integer literal"))
            }
        }

        Expr::Unary(expr) => {
            let value = eval_expr(&expr.expr)?;

            match expr.op {
                UnOp::Neg(_) => Ok(-value),
                UnOp::Not(_) => Ok(!value),
                _ => Err(syn::Error::new_spanned(expr, "Unsupported unary operator")),
            }
        }

        Expr::Binary(expr) => {
            let left = eval_expr(&expr.left)?;
            let right = eval_expr(&expr.right)?;

            match expr.op {
                BinOp::Add(_) => Ok(left + right),
                BinOp::Sub(_) => Ok(left - right),
                BinOp::Mul(_) => Ok(left * right),
                BinOp::Div(_) => {
                    if right == 0 {
                        return Err(syn::Error::new_spanned(expr, "Division by zero"));
                    }
                    Ok(left / right)
                }
                BinOp::Rem(_) => Ok(left % right),
                BinOp::BitXor(_) => Ok(left ^ right),
                BinOp::BitAnd(_) => Ok(left & right),
                BinOp::BitOr(_) => Ok(left | right),
                BinOp::Shl(_) => Ok(left << right),
                BinOp::Shr(_) => Ok(left >> right),
                _ => Err(syn::Error::new_spanned(expr, "Unsupported binary operator")),
            }
        }

        Expr::Paren(expr) => eval_expr(&expr.expr),
        Expr::Group(expr) => eval_expr(&expr.expr),

        _ => Err(syn::Error::new_spanned(
            expr,
            "Unsupported expression",
        )),
    }
}
//region A
struct PermutationForRangeNum {
    name: syn::Ident,
    min : i64,
    max : i64,
}
impl syn::parse::Parse for PermutationForRangeNum {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        input.parse::<syn::Token![,]>()?;

        let range: syn::ExprRange = input.parse()?;
        let min = range.start.as_ref().ok_or_else(|| syn::Error::new_spanned(&range, "Expected range start"))?;
        let max = range.end.as_ref().ok_or_else(|| syn::Error::new_spanned(&range, "Expected range end"))?;

        let half_open = matches!(range.limits, syn::RangeLimits::HalfOpen(_));
        let min = eval_expr(min)?;
        let mut max = eval_expr(max)?;

        if half_open {
            max = max.checked_sub(1).ok_or_else(|| {
                syn::Error::new_spanned(max.to_string(), "Range overflow")
            })?;
        }
        Ok(Self {
            name,
            min,
            max,
        })
    }
}
fn fill_right_bits(mut n: usize) -> usize {
    let mut old_n: usize = n^1;
    while old_n != n {
        old_n = n;
        n |= n>>1;
    }
    return n;
}
fn count_bits(n: usize) -> u8 {
    let mut count: u8 = 0;
    for i in (0u8)..(usize::BITS as u8) {
        if (n >> i)&1 != 0 {
            count += 1;
        }
    }
    return count;
}
#[proc_macro]
pub fn permutation_for_range_num(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let p = syn::parse_macro_input!(input as PermutationForRangeNum);
    let signed = p.min < 0;
    assert!(p.min < p.max);
    let bytes: usize = (
            count_bits(
                fill_right_bits(
                    p.max.abs() as usize
                )
            ) as usize
        )+(signed as usize)/8usize;
    let nt = match bytes {
        ..=8 => {
            quote::quote![i8]
        }
        ..=16 => {
            quote::quote![i16]
        }
        ..=32 => {
            quote::quote![i32]
        }
        ..=64 => {
            quote::quote![i64]
        }
        _ => {
            panic!()
        }
    };
    let unt = match bytes {
        ..=8 => {
            quote::quote![u8]
        }
        ..=16 => {
            quote::quote![u16]
        }
        ..=32 => {
            quote::quote![u32]
        }
        ..=64 => {
            quote::quote![u64]
        }
        _ => {
            panic!()
        }
    };
    let name = p.name;
    let max = p.max;
    let min = p.min;
    if signed {
        return quote::quote! {
            pub struct #name(#nt);
            impl #name {
                pub fn into_super(&self) -> #nt {
                    self.0
                }
            }
            impl From<#name> for #nt {
                fn from(value: #name) -> Self {
                    value.0
                }
            }
            impl From<#nt> for #name {
                fn from(value: #nt) -> Self {
                    Self(value)
                }
            }
            impl Permutation for #name {
                fn total_permutations() -> usize { ((#max as isize)-(#min as isize)+1).abs() as usize }
                fn current_permutation(&self) -> Option<usize> {
                    let a = (self.0 - #min) as usize;
                    if a > Self::total_permutations() {
                        return None;
                    }
                    return Some(((self.0 as #unt) - (#min as #unt)) as usize);
                }
                fn from_index(index: usize) -> Option<Self> {
                    if index <= Self::total_permutations() {
                        return Some(Self((index as #unt as #nt)+(#min as #unt as #nt)));
                    } else {
                        return None;
                    }
                }
            }
        }.into();
    } else {
        return quote::quote! {
            pub struct #name(#nt);
            impl #name {
                pub fn into(&self) -> #nt {
                    self.0
                }
                pub fn into_super(self) -> #nt {
                    self.0
                }
            }
            impl From<#name> for #nt {
                fn from(value: #name) -> Self {
                    value.0
                }
            }
            impl From<#nt> for #name {
                fn from(value: #nt) -> Self {
                    Self(value)
                }
            }
            impl Permutation for #name {
                fn total_permutations() -> usize { (#max as usize)-(#min as usize)+1 }
                fn current_permutation(&self) -> Option<usize> {
                    let a = (self.0 - #min) as usize;
                    if a > Self::total_permutations() {
                        return None;
                    }
                    return Some((self.0 - #min) as usize);
                }
                fn from_index(index: usize) -> Option<Self> {
                    if index <= Self::total_permutations() {
                        return Some(Self((index as #nt)+(#min as #nt)));
                    } else {
                        return None;
                    }
                }
            }
        }.into();
    }
}

fn encode(
    values: Vec<proc_macro2::TokenStream>,
    radices: Vec<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    assert_eq!(values.len(), radices.len());

    let operations = values.iter().zip(radices.iter()).map(
        |(value, radix)| {
            quote::quote! {
                MACRO__encode__index =
                    MACRO__encode__index
                        .checked_mul(#radix)?
                        .checked_add(#value)?;
            }
        }
    );

    quote::quote! {
        let mut MACRO__encode__index = 0usize;
        #(#operations)*
        return Some(MACRO__encode__index);
    }
}

fn decode(
    in_: syn::Ident,
    values: Vec<proc_macro2::TokenStream>,
    value_types: Vec<proc_macro2::TokenStream>,
    radices: Vec<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    assert_eq!(values.len(), radices.len());

    let operations = values.iter()
                     .zip(value_types.iter())
                     .zip(radices.iter()).rev().map(
        |((value, value_type), radix)| {
            quote::quote! {
                #value = #value_type::from_index(#in_ % #radix)?;
                #in_ /= #radix;
            }
        }
    );

    quote::quote! {
        #(#operations)*
    }
}
#[proc_macro_derive(PermutationStruct)]
pub fn permutation_struct(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input as syn::DeriveInput);

    let struct_name = &input.ident;

    let fields = match &input.data {
        syn::Data::Struct(data) => &data.fields,
        _ => {
            return syn::Error::new_spanned(input, "expected an struct")
                .to_compile_error()
                .into();
        }
    };
    let mut a = 0;
    let member_idents: Vec<_> = fields.iter().map(|_| {
        a+=1;
        quote::format_ident!("MACRO_PermutationStruct__m{}", a.to_string())
    }).collect();
    let mut prev_count = quote::quote! { 0usize };
    let mut field_counts = Vec::new();
    let lets: Vec<_> = fields.iter().zip(member_idents.iter()).map(|(i, m)| {
        let ftype = &i.ty;
        quote::quote! { let mut #m: #ftype; }
    }).collect();
    let idents: Vec<_> = fields.iter().zip(member_idents.iter()).map(|(i, m)| {
        let ident = &i.ident;
        quote::quote! { #ident: #m, }
    }).collect();
    for field in fields.iter() {
        let ty = &field.ty;
        let count = {
            quote::quote! {#ty::total_permutations()}
        };
        prev_count = quote::quote! { (#prev_count) + (#count) };
        field_counts.push(count);
    }

    let total = quote::quote! {
        1usize #(* #field_counts)*
    };

    let intos = encode(
        fields.iter().map(|field| {
            let ident = &field.ident;
            quote::quote!{ self.#ident.current_permutation()? }
        }).collect(),
        fields.iter().map(|field| {
            let ty = &field.ty;
            quote::quote!{ #ty::total_permutations() }
        }).collect()
    );
    let froms = decode(
        quote::format_ident!("MACRO__PermutationStruct__i"),
        member_idents.iter().map(|i| {
            quote::quote!{#i}
        }).collect(),
        fields.iter().map(|field| {
            let ftype = &field.ty;
            quote::quote!{#ftype}
        }).collect(),
        fields.iter().map(|field| {
            let ty = &field.ty;
            quote::quote!{ #ty::total_permutations() }
        }).collect()
    );

    let a = quote::quote! {
        impl crate::lpermutations::Permutation for #struct_name {
            fn total_permutations() -> usize {
                #total
            }
            fn current_permutation(&self) -> Option<usize> { 
                #intos
            }
            fn from_index(i: usize) -> Option<Self> {
                let mut MACRO__PermutationStruct__i: usize = i;
                if MACRO__PermutationStruct__i <= Self::total_permutations() {
                    #(#lets)*
                    #froms
                    //#(#froms)*
                    return Some(Self {
                        #(#idents)*
                    });
                }
                return None;
            }
        }
    };
    println!("{}", a);
    a.into()
}