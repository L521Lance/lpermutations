pub type Psize = u128;
pub type Pisize = i128;

pub trait Permutation: Sized {
    fn total_permutations() -> Psize;
    fn current_permutation(&self) -> Option<Psize>;
    fn from_index(index: Psize) -> Option<Self>;

    fn into_iter() -> impl Iterator<Item = Self> {
        (0..Self::total_permutations()).filter_map(Self::from_index)
    }

    fn all_iter() -> impl Iterator<Item = Self> {
        (0..Self::total_permutations()).filter_map(Self::from_index)
    }

    fn range_iter(start: Psize, end: Psize) -> impl Iterator<Item = Self> {
        assert!(!(start > end));
        assert!(end <= Self::total_permutations());
        (start..end).filter_map(Self::from_index)
    }

    fn previous_permutation(&self) -> Option<Self> {
        Self::from_index(self.current_permutation()?.checked_sub(1)?)
    }

    fn next_permutation(&self) -> Option<Self> {
        Self::from_index(self.current_permutation()?.checked_add(1)?)
    }
    
    fn first_permutation() -> Self {
        Self::from_index(0).unwrap()
    }

    fn last_permutation() -> Self {
        Self::from_index(Self::total_permutations()-1).unwrap()
    }
}

macro_rules! permutation_for_num {
    ($nt:ty, $name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct $name(pub $nt);

        impl $name {
            pub fn into(&self) -> $nt {
                self.0
            }

            pub fn into_super(self) -> $nt {
                self.0
            }
        }

        impl From<$name> for $nt {
            fn from(value: $name) -> Self {
                value.0
            }
        }

        impl From<$nt> for $name {
            fn from(value: $nt) -> Self {
                Self(value)
            }
        }

        impl Permutation for $name {
            fn total_permutations() -> Psize {
                (<$nt>::MAX as Psize + 1)
                    .try_into()
                    .expect("numeric range does not fit Psize")
            }

            fn current_permutation(&self) -> Option<Psize> {
                (self.0 as Psize).try_into().ok()
            }

            fn from_index(index: Psize) -> Option<Self> {
                if index as Psize > <$nt>::MAX as Psize {
                    return None;
                }

                Some(Self(index as $nt))
            }
        }
    };
}

macro_rules! permutation_for_signed_num {
    ($nt:ty, $unt:ty, $name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct $name(pub $nt);
        impl $name {
            pub fn into(&self) -> $nt {
                self.0
            }
            pub fn into_super(self) -> $nt {
                self.0
            }
        }
        impl From<$name> for $nt {
            fn from(value: $name) -> Self {
                value.0
            }
        }
        impl From<$nt> for $name {
            fn from(value: $nt) -> Self {
                Self(value)
            }
        }
        impl Permutation for $name {
            fn total_permutations() -> Psize {
                ((<$nt>::MAX as Psize) * 2 + 2)
                    .try_into()
                    .expect("numeric range does not fit Psize")
            }
            fn current_permutation(&self) -> Option<Psize> {
                let offset = (self.0 as Pisize) - (<$nt>::MIN as Pisize);
                Psize::try_from(offset).ok()
            }

            fn from_index(index: Psize) -> Option<Self> {
                let offset = index as Pisize;
                let value = (<$nt>::MIN as Pisize).checked_add(offset)?;

                if value > <$nt>::MAX as Pisize {
                    return None;
                }

                Some(Self(value as $nt))
            }
        }
    };
}

permutation_for_num!(u8, Pu8);
permutation_for_num!(u16, Pu16);
permutation_for_num!(u32, Pu32);
permutation_for_num!(u64, Pu64);

permutation_for_signed_num!(i8, u8, Pi8);
permutation_for_signed_num!(i16, u16, Pi16);
permutation_for_signed_num!(i32, u32, Pi32);
permutation_for_signed_num!(i64, u64, Pi64);