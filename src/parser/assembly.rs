//! アセンブリの構文解析

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Rule {
    operands: Box<[OperandRule]>,
    code: Box<[u8]>,
    opr_code: Box<[OprCode]>
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
    Address,
}

/// テーブルの使用を各ビットで表す
/// 第0ビットは即値を許すかどうか
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TableKey {
    index: NonZeroU64,
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

enum Operand {
    Register(Box<str>),
    Immediate(u64),
    Label(Box<str>),
}

impl Rule {
    
}