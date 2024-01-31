pub mod lex;
pub mod parser;

// private module
#[allow(dead_code)]
mod bit_slice {

    #[derive(Default)]
    pub struct Bin;
    #[derive(Default)]
    pub struct Hex;

    pub struct SeparateWord(u8);

    pub trait PrinterSlice {
        fn print(&self, src: &[u8], f: &mut fmt::Formatter<'_>) -> fmt::Result;
    }

    fn print_slice(
        src: &[u8],
        f: &mut fmt::Formatter<'_>,
        mut print: impl FnMut(u8, &mut fmt::Formatter<'_>) -> fmt::Result,
    ) -> fmt::Result {
        write!(f, "[")?;
        let mut iterator = src.iter();
        if let Some(i) = iterator.next() {
            print(*i, f)?;
            for i in iterator {
                write!(f, ", ")?;
                print(*i, f)?;
            }
        }
        write!(f, "]")
    }

    impl PrinterSlice for Bin {
        fn print(&self, src: &[u8], f: &mut fmt::Formatter<'_>) -> fmt::Result {
            print_slice(src, f, |i, f| write!(f, "{:08b}", i))
        }
    }

    impl PrinterSlice for Hex {
        fn print(&self, src: &[u8], f: &mut fmt::Formatter<'_>) -> fmt::Result {
            print_slice(src, f, |i, f| write!(f, "{:02x}", i))
        }
    }

    impl PrinterSlice for SeparateWord {
        fn print(&self, src: &[u8], f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut count = 0;
            for v in src {
                for i in 0u8..8 {
                    if *v & (0b10000000 >> i) != 0 {
                        write!(f, "1")?;
                    } else {
                        write!(f, "0")?;
                    }
                    count += 1;
                    if count == self.0 {
                        write!(f, " ")?;
                        count = 0;
                    }
                }
            }
            Ok(())
        }
    }

    pub struct SliceBit<'a, T: PrinterSlice> {
        slc: &'a [u8],
        printer: T,
    }

    impl<'a, T: PrinterSlice> PartialEq for SliceBit<'a, T> {
        fn eq(&self, other: &Self) -> bool {
            self.slc == other.slc
        }
    }

    use std::fmt;
    use std::ops::Deref;
    impl<'a, T: PrinterSlice> fmt::Debug for SliceBit<'a, T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.printer.print(self.slc, f)
        }
    }
    impl<'a, T: PrinterSlice> Deref for SliceBit<'a, T> {
        type Target = [u8];

        fn deref(&self) -> &Self::Target {
            self.slc
        }
    }

    pub fn slice_bit<T: PrinterSlice + Default>(slc: &[u8]) -> SliceBit<T> {
        SliceBit {
            slc,
            printer: T::default(),
        }
    }

    pub fn separate_into_word(slc: &[u8], word_size: u8) -> SliceBit<SeparateWord> {
        SliceBit {
            slc,
            printer: SeparateWord(word_size),
        }
    }
}
