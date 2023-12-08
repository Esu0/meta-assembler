//! アセンブリの構文解析

use itertools::Itertools;
use std::{collections::HashMap, num::NonZeroU64};
/// アセンブリのコード生成ルール全体を表す。
/// ニーモニック一つにルール一つが対応するようになっている。
/// # 注意
/// アドレスモードがリトルエンディアンのとき、
/// 以下の全てを満たすオペランドのルールが存在してはならない
/// * オペランドにより定まるビットの長さがワードサイズの倍数でない
/// * 即値またはラベルによる指定が可能である。
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Rules {
    rules: HashMap<Box<str>, Rule>,
    general: GeneralRule,
}

#[derive(Debug, thiserror::Error)]
#[error("設定{rule_name}は既に存在します。")]
pub struct DoubleDefinitionError {
    rule_name: Box<str>,
}

impl From<DoubleDefinitionError> for super::ErrorKind {
    fn from(value: DoubleDefinitionError) -> Self {
        Self::AlreadyExsistentProperty {
            found: value.rule_name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct RulesConfig {
    rules: HashMap<Box<str>, Rule>,
}

impl GeneralRuleConfig {
    fn set_value(dst: &mut Option<u8>, src: u8) -> Result<(), DoubleDefinitionError> {
        if let Some(dst) = dst {
            Err(DoubleDefinitionError {
                rule_name: "general".into(),
            })
        } else {
            *dst = Some(src);
            Ok(())
        }
    }
    pub fn set_word_size(&mut self, word_size: u8) -> Result<(), DoubleDefinitionError> {
        Self::set_value(&mut self.word_size, word_size)
    }

    pub fn set_address_size(&mut self, address_size: u8) -> Result<(), DoubleDefinitionError> {
        Self::set_value(&mut self.address_size, address_size)
    }

    pub fn set_address_mode(&mut self, address_mode: u8) -> Result<(), DoubleDefinitionError> {
        Self::set_value(&mut self.address_mode, address_mode)
    }

    pub fn set_empty_symbol_mode(
        &mut self,
        empty_symbol_mode: u8,
    ) -> Result<(), DoubleDefinitionError> {
        Self::set_value(&mut self.empty_symbol_mode, empty_symbol_mode)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GeneralRuleConfig {
    pub word_size: Option<u8>,
    pub address_size: Option<u8>,
    pub address_mode: Option<u8>,
    pub empty_symbol_mode: Option<u8>,
}

/// 一般的な規則の定義
#[derive(Debug, Clone, PartialEq, Eq)]
struct GeneralRule {
    word_size: u8,
    address_size: u8,
    address_mode: u8,
    empty_symbol_mode: u8,
}

impl From<GeneralRuleConfig> for GeneralRule {
    fn from(value: GeneralRuleConfig) -> Self {
        Self {
            word_size: value.word_size.unwrap_or(8),
            address_size: value.address_size.unwrap_or(16),
            address_mode: value.address_mode.unwrap_or(2),
            empty_symbol_mode: value.empty_symbol_mode.unwrap_or(0),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SyntaxRuleConfig {
    pub org: Option<Box<str>>,
    pub end: Option<Box<str>>,
    pub equ: Option<Box<str>>,
    pub db: Option<Box<str>>,
    pub ds: Option<Box<str>>,
    pub dc: Option<Box<str>>,
    pub code_dot: Option<char>,
    pub operand_dot: Option<char>,
    pub dc_dot: Option<char>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SyntaxRule {
    org: Box<str>,
    end: Box<str>,
    equ: Option<Box<str>>,
    db: Option<Box<str>>,
    ds: Option<Box<str>>,
    dc: Option<Box<str>>,
    code_dot: char,
    operand_dot: char,
    dc_dot: char,
}

impl From<SyntaxRuleConfig> for SyntaxRule {
    fn from(value: SyntaxRuleConfig) -> Self {
        Self {
            org: value.org.unwrap_or_else(|| "ORG".into()),
            end: value.end.unwrap_or_else(|| "END".into()),
            equ: value.equ,
            db: value.db,
            ds: value.ds,
            dc: value.dc,
            code_dot: value.code_dot.unwrap_or('.'),
            operand_dot: value.operand_dot.unwrap_or(','),
            dc_dot: value.dc_dot.unwrap_or('"'),
        }
    }
}

impl Default for SyntaxRule {
    fn default() -> Self {
        Self {
            org: "ORG".into(),
            end: "END".into(),
            equ: None,
            db: None,
            ds: None,
            dc: None,
            code_dot: '.',
            operand_dot: ',',
            dc_dot: '"',
        }
    }
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

/// コード生成ルール
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    operands: Box<[OperandRule]>,
    /// オペコードのビット列
    /// オペランドがない時のビット列になるようにする
    /// 配列の後ろの方が小さいアドレスのデータを指すように配置する
    code: Box<[u8]>,
    opr_code: Box<[OprCode]>,
    /// 相対/絶対アドレッシング(ラベルによる指定のときのみ有効)
    relative: bool,
}

/// gmetaでいう`opr(1:16)`のような構文に対応する概念
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OprCode {
    /// オペランドのインデックス
    opr: u8,
    /// オペランドのコードの長さ
    length: u8,
    /// オペランドのコードの開始位置
    position: u16,
}

/// テーブルによる指定は通常、レジスタの指定などに使われるため、アドレスの指定に
/// 使うことはないと判断している。そのため、
/// テーブルによるオペランドの指定とラベルによるオペランドの指定は相容れないものとしている。
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OperandRule {
    /// レジスタ指定など。ラベルを許さない。
    Register(TableKey),
    /// ラベルまたは即値でのみ指定可能なオペランド。テーブルを参照できない。
    /// アドレスを指定するときに使用する想定。
    Address,
}

/// テーブルの使用を各ビットで表す
/// 第0ビットは即値を許すかどうか
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TableKey {
    index: NonZeroU64,
}

impl TableKey {
    fn indice(self) -> TableKeyIndice {
        TableKeyIndice {
            index: self.index,
            bit: 1,
        }
    }

    pub fn from_indice(slc: &[u8]) -> Self {
        let mut ind = 0;
        for i in slc {
            if *i >= 63 {
                panic!("index must be less than 63");
            }
            ind |= 1 << (i + 1);
        }
        Self {
            index: NonZeroU64::new(ind).expect("indice slice must not be empty"),
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
pub struct Tables {
    tbls: Box<[Table]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table {
    // R0,R1などの名前と値の対応
    tbl: HashMap<Box<str>, u64>,
}

impl Table {
    pub fn from_slice(slc: &[(&str, u64)]) -> Self {
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
/// lenがbyte_lenの倍数でなければならない
fn little_endian(mut n: u64, mut len: u8, byte_len: u8) -> std::result::Result<u64, u64> {
    if len % byte_len == 0 {
        let mut result = 0;
        let msk = (1 << byte_len) - 1;
        while len > 0 {
            result <<= byte_len;
            result |= n & msk;
            n >>= byte_len;
            len -= byte_len;
        }
        Ok(result)
    } else {
        Err(n)
    }
}

impl Rule {
    fn assemble(
        &self,
        operands: impl Iterator<Item = Option<Operand>>,
        table: &Tables,
        labels: &LabelTable,
        base_address: u64,
        general_rule: &GeneralRule,
    ) -> Result<Box<[u8]>, Error> {
        let opr_bytes: Vec<Option<(u64, bool)>> = operands
            .zip(self.operands.iter())
            .map(|(opr, rule)| match (opr, rule) {
                (Some(Operand::Register(r)), OperandRule::Register(key)) => {
                    if let Some(n) = key
                        .indice()
                        .filter_map(|i| table.tbls[i as usize].tbl.get(&r))
                        .copied()
                        .next()
                    {
                        Ok(Some((n, false)))
                    } else {
                        Err(Error::op_violation(OperandViolation::UnknownRegister(
                            r.clone(),
                        )))
                    }
                }
                (Some(Operand::Immediate(n)), rule) => match rule {
                    OperandRule::Address => Ok(Some((n, true))),
                    OperandRule::Register(k) if k.index.get() & 1 == 1 => Ok(Some((n, true))),
                    _ => Err(Error::op_violation(OperandViolation::CannotUseImmediate)),
                },
                (Some(Operand::Label(l)), OperandRule::Address) => {
                    if let Some(n) = labels.tbl.get(&l).copied() {
                        Ok(Some((
                            if self.relative {
                                n.wrapping_sub(base_address)
                            } else {
                                n
                            },
                            true,
                        )))
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
            if let Some((n, le)) = opr_bytes[opr_code.opr as usize] {
                let n = if le {
                    little_endian(n, opr_code.length, general_rule.word_size)
                        .unwrap_or_else(|_| unreachable!())
                } else {
                    n
                };
                write_bits(&mut result, n, opr_code.position as usize, opr_code.length);
            }
        }
        Ok(result)
    }
}

pub struct Parser<T> {
    tokens: T,
}

use thiserror::Error;

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
    pub fn op_violation(kind: OperandViolation) -> Self {
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
                OperandRule::Address,
            ]
            .into(),
            code: Box::new([0x00, 0x00, 0x00, 0xA0]),
            opr_code: Box::new([
                OprCode {
                    opr: 0,
                    length: 3,
                    position: 18,
                },
                OprCode {
                    opr: 1,
                    length: 16,
                    position: 0,
                },
            ]),
            relative: false,
        };
        let general_rule = GeneralRule {
            word_size: 8,
            address_size: 16,
            address_mode: 2,
            empty_symbol_mode: 0,
        };
        let labels = LabelTable {
            tbl: Default::default(),
        };
        let result = rule.assemble(
            [
                Some(Operand::Register("R3".to_owned().into())),
                Some(Operand::Immediate(0x0a0f)),
            ]
            .into_iter(),
            &tables,
            &labels,
            0,
            &general_rule,
        );

        match result {
            Ok(result) => {
                for byte in result.iter().rev().copied() {
                    print!("{:08b} ", byte);
                }
                println!();
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    }
}
