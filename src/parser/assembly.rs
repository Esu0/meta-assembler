//! アセンブリの構文解析

use itertools::Itertools;
use std::{collections::HashMap, num::NonZeroU64};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Rules {
    rules: HashMap<Box<str>, Rule>,
    general: GeneralRule,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct GeneralRule {
    word_size: u8,
    address_size: u8,
    address_mode: u8,
    empty_symbol_mode: u8,
}

impl Default for GeneralRule {
    fn default() -> Self {
        Self {
            word_size: 8,
            address_size: 16,
            address_mode: 2,
            empty_symbol_mode: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Rule {
    operands: Box<[OperandRule]>,
    /// オペコードのビット列
    /// オペランドがない時のビット列になるようにする
    code: Box<[u8]>,
    opr_code: Box<[OprCode]>,
    /// アドレッシングモード(ラベルによる指定のときのみ有効)
    relative: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OprCode {
    /// オペランドのインデックス
    opr: u8,
    /// オペランドのコードの長さ
    length: u8,
    /// オペランドのコードの開始位置
    position: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum OperandRule {
    /// レジスタ指定など。ラベルを許さない
    Register(TableKey),
    /// 即値。テーブルを参照できない
    Immediate,
}

/// テーブルの使用を各ビットで表す
/// 第0ビットは即値を許すかどうか
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TableKey {
    index: NonZeroU64,
}

impl TableKey {
    fn indice(self) -> TableKeyIndice {
        TableKeyIndice {
            index: self.index,
            bit: 1,
        }
    }
}

struct TableKeyIndice {
    index: NonZeroU64,
    bit: u8,
}

impl Iterator for TableKeyIndice {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        while self.index.get() & (1 << self.bit) == 0 {
            self.bit += 1;
            if self.bit >= 64 {
                return None;
            }
        }
        let tmp = self.bit;
        self.bit += 1;
        Some(tmp - 1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Tables {
    tbls: Box<[Table]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Table {
    // R0,R1などの名前と値の対応
    tbl: HashMap<Box<str>, u64>,
}

impl Table {
    fn from_slice(slc: &[(&str, u64)]) -> Self {
        Self {
            tbl: slc.iter().map(|(k, v)| (k.to_owned().into(), *v)).collect(),
        }
    }
}

enum Operand {
    Register(Box<str>),
    Immediate(u64),
    Label(Box<str>),
}

struct LabelTable {
    tbl: HashMap<Box<str>, u64>,
}

fn write_bits(slc: &mut [u8], mut n: u64, pos: usize, mut len: u8) {
    n &= (1 << len) - 1;
    let mut il = pos / 8;
    let jl = (pos % 8) as u8;
    if jl + len > 8 {
        slc[il] = (slc[il] & !(0xff << jl)) | (n as u8) << jl;
        len -= 8 - jl;
        il += 1;
        n >>= 8 - jl;
        while len >= 8 {
            slc[il] = n as u8;
            n >>= 8;
            len -= 8;
            il += 1;
        }
        if len > 0 {
            slc[il] = (slc[il] & (0xff << len)) | (n as u8);
        }
    } else {
        slc[il] = (slc[il] & !(0xff << jl)) | ((n as u8 & ((1 << len) - 1)) << jl);
    }
}

/// エンディアン変換
fn toggle_endian(n: u64, len: u8, byte_len: u8) -> u64 {
    unimplemented!()
}

impl Rule {
    fn assemble(
        &self,
        operands: impl Iterator<Item = Option<Operand>>,
        table: &Tables,
        labels: &LabelTable,
        base_address: u64,
    ) -> Result<Box<[u8]>> {
        let opr_bytes: Vec<Option<u64>> = operands
            .zip(self.operands.iter())
            .map(|(opr, rule)| match (opr, rule) {
                (Some(Operand::Register(r)), OperandRule::Register(key)) => {
                    if let Some(n) = key
                        .indice()
                        .filter_map(|i| table.tbls[i as usize].tbl.get(&r))
                        .copied()
                        .next()
                    {
                        Ok(Some(n))
                    } else {
                        Err(Error::op_violation(OperandViolation::UnknownRegister(
                            r.clone(),
                        )))
                    }
                }
                (Some(Operand::Immediate(n)), rule) => match rule {
                    OperandRule::Immediate => Ok(Some(n)),
                    OperandRule::Register(k) if k.index.get() & 1 == 1 => Ok(Some(n)),
                    _ => Err(Error::op_violation(OperandViolation::CannotUseImmediate)),
                },
                (Some(Operand::Label(l)), OperandRule::Immediate) => {
                    if let Some(n) = labels.tbl.get(&l).copied() {
                        if self.relative {
                            Ok(Some(n.wrapping_sub(base_address)))
                        } else {
                            Ok(Some(n))
                        }
                    } else {
                        Err(Error::op_violation(OperandViolation::UnknownLabel(
                            l.clone(),
                        )))
                    }
                }
                (Some(Operand::Register(_)), _) => {
                    Err(Error::op_violation(OperandViolation::CannotUseRegister))
                }
                (Some(Operand::Label(_)), _) => {
                    Err(Error::op_violation(OperandViolation::CannotUseLabel))
                }
                (None, _) => Ok(None),
            })
            .try_collect()?;
        let mut result: Box<[u8]> = self.code.clone();
        for opr_code in &self.opr_code[..] {
            if let Some(n) = opr_bytes[opr_code.opr as usize] {
                write_bits(&mut result, n, opr_code.position as usize, opr_code.length);
            }
        }
        Ok(result)
    }
}

use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error(transparent)]
    OperandViolation(#[from] OperandViolation),
}

#[derive(Debug, Error)]
pub enum OperandViolation {
    #[error("\"{0}\"に一致する識別子がテーブルから見つかりません")]
    UnknownRegister(Box<str>),
    #[error("\"{0}\"に一致するラベルは存在しません")]
    UnknownLabel(Box<str>),
    #[error("即値は使用できません")]
    CannotUseImmediate,
    #[error("ラベルは使用できません")]
    CannotUseLabel,
    #[error("テーブルの識別子は使用できません")]
    CannotUseRegister,
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct Error {
    #[from]
    kind: ErrorKind,
}

impl Error {
    fn op_violation(kind: OperandViolation) -> Self {
        Self {
            kind: ErrorKind::OperandViolation(kind),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn assemble_test() {
        let tables = Tables {
            tbls: vec![
                Table::from_slice(&[
                    ("R0", 0),
                    ("R1", 1),
                    ("R2", 2),
                    ("R3", 3),
                    ("R4", 4),
                    ("R5", 5),
                ]),
                Table::from_slice(&[("X0", 0), ("X1", 1), ("X2", 2)]),
            ]
            .into(),
        };
        let rule = Rule {
            operands: vec![
                OperandRule::Register(TableKey {
                    index: NonZeroU64::new(0b10).unwrap(),
                }),
                OperandRule::Immediate,
            ]
            .into(),
            code: Box::new([0xA0, 0x00, 0x00, 0x00]),
            opr_code: Box::new([
                OprCode {
                    opr: 0,
                    length: 3,
                    position: 11,
                },
                OprCode {
                    opr: 1,
                    length: 16,
                    position: 16,
                },
            ]),
            relative: false,
        };
        let labels = LabelTable {
            tbl: Default::default(),
        };
        let result = rule.assemble(
            [
                Some(Operand::Register("X0".to_owned().into())),
                Some(Operand::Immediate(0x0f0f)),
            ]
            .into_iter(),
            &tables,
            &labels,
            0,
        );

        match result {
            Ok(result) => {
                for byte in result.iter().copied() {
                    print!("{:08b} ", byte);
                }
                println!();
            },
            Err(e) => {
                println!("{}", e);
            }
        }
    }
}
