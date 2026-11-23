/* #region PermutationEnum */
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

    let mut prev_count = quote::quote! { (0 as ::lpermutations::Psize) };
    let mut variant_counts = Vec::new();
    let mut froms = Vec::new();
    let mut intos = Vec::new();

    for variant in variants {
        let variant_name = &variant.ident;
        let count = match &variant.fields {
            syn::Fields::Unit => quote::quote! { (1 as ::lpermutations::Psize) },
            syn::Fields::Unnamed(fields) => {
                let types = fields.unnamed.iter().map(|field| &field.ty);
                quote::quote! {(1 as ::lpermutations::Psize) #(* #types::total_permutations())*}
            }
            syn::Fields::Named(fields) => {
                let types = fields.named.iter().map(|field| &field.ty);
                quote::quote! {(1 as ::lpermutations::Psize) #(* #types::total_permutations())*}
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
                let mut offset = quote::quote! { (0 as ::lpermutations::Psize) };
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
                let mut offset = quote::quote! { (0 as ::lpermutations::Psize) };
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
                    return Some(#prev_count + a.current_permutation()?);
                }
            }
        );
        prev_count = quote::quote! { (#prev_count) + (#count) };
        variant_counts.push(prev_count.clone());
    }

    let total = quote::quote! {
        (0 as ::lpermutations::Psize) #(+ #variant_counts)*
    };

    quote::quote! {
        impl ::lpermutations::Permutation for #enum_name {
            fn total_permutations() -> ::lpermutations::Psize {
                #total
            }
            fn current_permutation(&self) -> Option<::lpermutations::Psize> { 
                match self {
                    #(#intos)*
                    _ => {
                        return None;
                    }
                }
            }
            fn from_index(n: ::lpermutations::Psize) -> Option<Self> {
                #(#froms)*
                return None;
            }
        }
    }.into()
}
/* #endregion */

/* #region permutation_for_range_num */
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
fn fill_right_bits<T: num_traits::PrimInt>(mut n: T) -> T {
    let mut old_n: T = n^T::one();
    while old_n != n {
        old_n = n;
        n = n|n>>1;
    }
    return n;
}
#[proc_macro]
pub fn permutation_for_range_num(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let p = syn::parse_macro_input!(input as PermutationForRangeNum);
    let signed = p.min < 0;
    assert!(p.min < p.max || p.min > p.max);
    let bits: usize = (
            fill_right_bits(
                p.max.abs() as usize
            ).count_ones() as usize
        )+(signed as usize + 7);
    let nt = match bits {
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
        ..=128 => {
            quote::quote![i128]
        }
        _ => {
            panic!()
        }
    };
    let unt = match bits {
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
        ..=128 => {
            quote::quote![u128]
        }
        _ => {
            panic!()
        }
    };
    let tt = if signed { &nt } else { &unt };
    let name = p.name;
    let max = p.max;
    let min = p.min;
    let backwards = min > max;
    let total_permutations = match (signed, backwards) {
        (true, false) => {quote::quote!{
            fn total_permutations() -> ::lpermutations::Psize {
                (#max as Pisize)-(#min as Pisize)+1
            }
        }}
        (false, false) => {quote::quote!{
            fn total_permutations() -> ::lpermutations::Psize {
                (#max as ::lpermutations::Psize)-(#min as ::lpermutations::Psize)+1
            }
        }}
        (true, true) => {quote::quote!{
            fn total_permutations() -> ::lpermutations::Psize {
                (#min as Pisize)-(#max as Pisize)+1
            }
        }}
        (false, true) => {quote::quote!{
            fn total_permutations() -> ::lpermutations::Psize {
                (#min as ::lpermutations::Psize)-(#max as ::lpermutations::Psize)+1
            }
        }}
    };
    let current_permutations = match backwards {
        false => {quote::quote!{
            fn current_permutation(&self) -> Option<::lpermutations::Psize> {
                let value = self.0 as #tt;
                let min = #min as #tt;
                let max = #max as #tt;

                if value < min || value > max {
                    return None;
                }

                Some((value - min) as ::lpermutations::Psize)
            }
        }}
        true => {quote::quote!{
            fn current_permutation(&self) -> Option<::lpermutations::Psize> {
                let value = self.0 as #tt;
                let max = #min as #tt;
                let min = #max as #tt;

                if value < min || value > max {
                    return None;
                }

                Some((max-value) as ::lpermutations::Psize)
            }
        }}
    };
    let from_index = match (signed, backwards) {
        (true, false) => {quote::quote!{
            fn from_index(index: ::lpermutations::Psize) -> Option<Self> {
                if index <= Self::total_permutations() {
                    return Some(Self((index as #unt as #nt)+(#min as #unt as #nt)));
                } else {
                    return None;
                }
            }
        }}
        (false, false) => {quote::quote!{
            fn from_index(index: ::lpermutations::Psize) -> Option<Self> {
                if index <= Self::total_permutations() {
                    return Some(Self((index as #unt)+(#min as #unt)));
                } else {
                    return None;
                }
            }
        }}
        (true, true) => {quote::quote!{
            fn from_index(index: ::lpermutations::Psize) -> Option<Self> {
                if index <= Self::total_permutations() {
                    return Some(Self((#min as #nt)-(index as #unt as #nt)+(#max as #unt as #nt)));
                } else {
                    return None;
                }
            }
        }}
        (false, true) => {quote::quote!{
            fn from_index(index: ::lpermutations::Psize) -> Option<Self> {
                if index <= Self::total_permutations() {
                    return Some(Self((#min as #unt)-(index as #unt)+(#max as #unt)));
                } else {
                    return None;
                }
            }
        }}
    };

    return quote::quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct #name(#tt);
        impl #name {
            pub fn into_super(&self) -> #tt {
                self.0
            }
        }
        impl From<#name> for #tt {
            fn from(value: #name) -> Self {
                value.0
            }
        }
        impl From<#tt> for #name {
            fn from(value: #tt) -> Self {
                Self(value)
            }
        }
        impl ::lpermutations::Permutation for #name {
            #total_permutations
            #current_permutations
            #from_index
        }
    }.into();
}
/* #endregion */

/* #region PermutationStruct */
#[proc_macro_derive(PermutationStruct)]
pub fn permutation_struct(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
            let mut MACRO__encode__index = (0 as ::lpermutations::Psize);
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
    let input: syn::DeriveInput = syn::parse_macro_input!(input as syn::DeriveInput);

    let struct_name = &input.ident;

    let fields: Vec<&_> = match &input.data {
        syn::Data::Struct(data) => &data.fields,
        _ => {
            return syn::Error::new_spanned(input, "expected an struct")
                .to_compile_error()
                .into();
        }
    }.iter().rev().collect();
    let mut a = 0;
    let member_idents: Vec<_> = fields.iter().map(|_| {
        a+=1;
        quote::format_ident!("MACRO_PermutationStruct__m{}", a.to_string())
    }).collect();
    let mut prev_count = quote::quote! { (0 as ::lpermutations::Psize) };
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
        (1 as ::lpermutations::Psize) #(* #field_counts)*
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

    quote::quote! {
        impl ::lpermutations::Permutation for #struct_name {
            fn total_permutations() -> ::lpermutations::Psize {
                #total
            }
            fn current_permutation(&self) -> Option<::lpermutations::Psize> { 
                #intos
            }
            fn from_index(i: ::lpermutations::Psize) -> Option<Self> {
                let mut MACRO__PermutationStruct__i: ::lpermutations::Psize = i;
                if MACRO__PermutationStruct__i <= Self::total_permutations() {
                    #(#lets)*
                    #froms
                    return Some(Self {
                        #(#idents)*
                    });
                }
                return None;
            }
        }
    }.into()
}
/* #endregion */

/* #region permutation_vec */
struct PermutationVec {
    name: syn::Ident,
    ty: syn::Type,
    size: syn::Expr,
}
impl syn::parse::Parse for PermutationVec {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let ty = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let size = input.parse()?;
        return Ok(Self {
            name: name,
            ty: ty,
            size: size
        })
    }
}
#[proc_macro]
pub fn permutation_vec(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    fn decode(
        ty: &syn::Type,
        size: usize,
    ) -> proc_macro2::TokenStream {
        let operations = std::iter::repeat_n(
            quote::quote! {{
                let r = #ty::from_index(
                    i % #ty::total_permutations()
                )?;
                i /= #ty::total_permutations();
                r
            }},
            size,
        );

        quote::quote! {
            Box::<[#ty; #size]>::new([#(#operations),*])
        }
    }
    let input = syn::parse_macro_input!(input as PermutationVec);
    let name = &input.name;
    let ty = &input.ty;
    let size = eval_expr(&input.size).unwrap();
    assert!(size >= 1, "Size must be at least 1");
    let mut total_permutations: Vec<_> = vec![quote::quote![(0 as ::lpermutations::Psize) + ]];
    for i in 1..size {
        let mut a: Vec<_> = vec![];
        for _ in 0..i {
            a.push(quote::quote! { #ty::total_permutations() });
            a.push(quote::quote![*]);
        }
        _ = a.pop();
        total_permutations.push(quote::quote![#(#a)*]);
        total_permutations.push(quote::quote![+]);
    }
    _ = total_permutations.pop();
    let total_permutations = quote::quote! { #(#total_permutations)* + 1 };
    let mut prev = quote::quote! {};
    let mut froms = quote::quote! {};
    let mut intos = quote::quote! {};
    for i in 1..=size {
        let end = if i == 1 {quote::quote! {
            #ty::total_permutations()
        }} else {quote::quote! {
            #ty::total_permutations().pow(#i as u32)+#ty::total_permutations()
        }};
        let d = decode(
            &ty,
            i as usize
        );
        let from = if i == 1i64 {quote::quote! {
            if i <= (#end as ::lpermutations::Psize) {
                i -= 1;
                return Some(Self {
                    array: #d
                })
            }
        }} else {quote::quote! {
            if i <= (#end as ::lpermutations::Psize) {
                i -= (#prev as ::lpermutations::Psize)+1;
                return Some(Self {
                    array: #d
                })
            }
        }};
        let intoo: Vec<_> = if i == 1 {
            vec![]
        } else {(0..i).into_iter().map(
            |j| {
                if j == 0 {
                    quote::quote! {
                        + self.array[#j as usize].current_permutation()?
                    }
                } else {
                    quote::quote! {
                        + #ty::total_permutations().pow(#j as u32)
                        * self.array[#j as usize].current_permutation()?
                    }
                }
            }
        ).collect()};
        let into = if i == 1 {
            quote::quote! {
                if l == 1usize {
                    return Some(
                        1 + self.array[0].current_permutation()?
                    );
                }
            }
        } else {
            quote::quote! {
                if l == (#i as usize) {
                    return Some(
                        ((#prev as ::lpermutations::Psize) + 1)
                        #(#intoo)*
                    );
                }
            }
        };
        froms = quote::quote! { #froms #from };
        intos = quote::quote! { #intos #into };
        prev = end;
    }
    let a = quote::quote! {
        struct #name {
            array: Box<[#ty]>
        }
        impl ::lpermutations::Permutation for #name {
            fn total_permutations() -> ::lpermutations::Psize {
                return #total_permutations;
            }
            fn current_permutation(&self) -> Option<::lpermutations::Psize> {
                let l = self.array.len();
                if l == 0 {
                    return Some(0);
                }
                if l >= (#size as usize) {
                    return None;
                }

                #intos
                
                None
            }
            fn from_index(i: ::lpermutations::Psize) -> Option<Self> {
                let mut i = i;
                if i == 0 {
                    return Some(Self{array: Box::<[#ty; 0]>::new([])})
                }
                if i > Self::total_permutations() {
                    return None;
                }
                #froms
                return None;
            }
        }
    };
    a.into()
}
/* #endregion */