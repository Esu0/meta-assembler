//! アセンブリの構文解析

pub mod parser;

use std::{collections::HashMap, num::NonZeroU64, ops};
/// アセンブリのコード生成ルール全体を表す。
/// ニーモニック一つにルール一つが対応するようになっている。
/// # 注意
/// アドレスモードがリトルエンディアンでも、
/// ビッグエンディアンで格納される場合もある。
/// リトルエンディアンで格納されるのは以下の条件をすべて満たしたときである。
/// * オペランドにより定まるビットの長さがワードサイズの倍数である
/// * オペランドにより定まるビットの格納位置がワードの先頭からである
/// * テーブルによる指定が不可能である
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Rules {
    rules: HashMap<Box<str>, Rule>,
    general: GeneralRule,
}

/// 二重定義エラー
#[derive(Debug)]
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

/// クレートの外部で[`Rules`]を構築するための構造体
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct RulesConfig {
    rules: HashMap<Box<str>, Rule>,
}

/// クレートの外部で[`GeneralRule`]を構築するための構造体
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GeneralRuleConfig {
    pub word_size: Option<u8>,
    pub address_size: Option<u8>,
    pub immediate_size: Option<u8>,
    pub address_mode: Option<u8>,
    pub empty_symbol_mode: Option<u8>,
}

impl GeneralRuleConfig {
    fn set_value(dst: &mut Option<u8>, src: u8) -> Result<(), DoubleDefinitionError> {
        if dst.is_some() {
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

    /// [`Self::address_size`]は[`Self::word_size`]の倍数である必要があるが、
    /// この実装では判定ができない。実装の変更を予定している。
    pub fn set_address_size(&mut self, address_size: u8) -> Result<(), DoubleDefinitionError> {
        Self::set_value(&mut self.address_size, address_size)
    }

    /// [`Self::immediate_size`]は[`Self::word_size`]の倍数である必要があるが、
    /// この実装では判定ができない。実装の変更を予定している。
    pub fn set_immediate_size(&mut self, immediate_size: u8) -> Result<(), DoubleDefinitionError> {
        Self::set_value(&mut self.immediate_size, immediate_size)
    }

    /// Invalidな値でエラーを返すように変更する必要あり
    pub fn set_address_mode(&mut self, address_mode: u8) -> Result<(), DoubleDefinitionError> {
        Self::set_value(&mut self.address_mode, address_mode)
    }

    /// Invalidな値でエラーを返すように変更する必要あり
    pub fn set_empty_symbol_mode(
        &mut self,
        empty_symbol_mode: u8,
    ) -> Result<(), DoubleDefinitionError> {
        Self::set_value(&mut self.empty_symbol_mode, empty_symbol_mode)
    }
}

/// 一般的な規則の定義
#[derive(Debug, Clone, PartialEq, Eq)]
struct GeneralRule {
    /// 1ワードのビット数
    word_size: u8,
    /// アドレスのワード数
    address_size_word: u8,
    /// 即値のワード数
    immediate_size_word: u8,
    /// アドレスモード
    address_mode: u8,
    /// オペコードやオペランドが省略されたときの扱い
    empty_symbol_mode: u8,
}

impl From<GeneralRuleConfig> for GeneralRule {
    fn from(value: GeneralRuleConfig) -> Self {
        let word_size = value.word_size.unwrap_or(8);
        let address_size_word = value.address_size.map_or(2, |v| v / word_size);
        Self {
            word_size,
            address_size_word,
            immediate_size_word: value
                .immediate_size
                .map_or(address_size_word, |v| v / word_size),
            address_mode: value.address_mode.unwrap_or(2),
            empty_symbol_mode: value.empty_symbol_mode.unwrap_or(0),
        }
    }
}

impl Default for GeneralRule {
    fn default() -> Self {
        Self {
            word_size: 8,
            address_size_word: 2,
            immediate_size_word: 2,
            address_mode: 2,
            empty_symbol_mode: 0,
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

/// コード生成ルール
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    operands: Box<[OperandRule]>,
    /// オペコードのビット列
    /// オペランドがない時のビット列になるようにする
    code: Box<[u8]>,
    /// 各オペランドがどのビットに作用するかを示すデータ
    opr_code: Box<[OprCode]>,
    /// 相対/絶対アドレッシング(ラベルによる指定のときのみ有効)
    relative: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct RuleConfig {
    pub operands: Vec<OperandRule>,
    pub code: Vec<u8>,
    pub opr_code: Vec<OprCode>,
    pub relative: bool,
}

/// gmetaでいう`opr(1:16)`のような構文に対応する概念
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OprCode {
    /// オペランドのインデックス
    ///
    /// [`Rule::operands`]におけるインデックス
    opr: u8,
    /// オペランドのコードの長さ
    length: u8,
    /// オペランドのコードの開始位置
    position: u16,
}

impl OprCode {
    fn new(opr: u8, length: u8, position: u16) -> Self {
        Self {
            opr,
            length,
            position,
        }
    }
}

/// テーブルによる指定は通常、レジスタの指定などに使われるため、アドレスの指定に
/// 使うことはないと判断している。そのため、
/// テーブルによるオペランドの指定とラベルによるオペランドの指定は相容れないものとしている。
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

    pub fn add_index(&mut self, index: u8) {
        if index >= 63 {
            panic!("index must be less than 63");
        }
        self.index = NonZeroU64::new(self.index.get() | 1 << (index + 1)).unwrap();
    }

    pub fn from_index(index: u8) -> Self {
        if index >= 63 {
            panic!("index must be less than 63");
        }
        Self {
            index: unsafe { NonZeroU64::new_unchecked((1 << (index + 1)) | 1) },
        }
    }

    /// 即値を禁止してテーブルのみ使用可能にする。
    /// 使用できるテーブルが無い場合は失敗し、`false`を返す。
    pub fn disable_immediate(&mut self) -> bool {
        if let Some(new) = NonZeroU64::new(self.index.get() & !1) {
            self.index = new;
            true
        } else {
            false
        }
    }

    /// 即値のみ使用可能なら`true`、一つでもテーブルが使用可能または即値が使用不可能なら`false`
    pub fn is_immediate_only(&self) -> bool {
        self.index.get() == 1
    }
}

pub mod table_key {
    pub fn add_index(table_key: &mut Option<super::TableKey>, index: u8) {
        if let Some(table_key) = table_key {
            table_key.add_index(index);
        } else {
            *table_key = Some(super::TableKey::from_index(index));
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

const MAX_TABLE_BIT_LENGTH_BYTE: usize = 8;
const _: () = assert!(MAX_TABLE_BIT_LENGTH_BYTE <= 16);

/// テーブルによって指定されるビット列を表す構造体
///
/// この構造体の参照は`&[u8]`として扱える
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TableBits([u8; MAX_TABLE_BIT_LENGTH_BYTE]);

impl TableBits {
    pub fn from_num(n: u128, length: u8) -> Self {
        assert!(
            length <= MAX_TABLE_BIT_LENGTH_BYTE as u8 * 8,
            "length must be less than or equal to MAX_TABLE_BIT_LENGTH_BYTE * 8"
        );
        Self(unsafe {
            n.checked_shl((128 - length) as _)
                .unwrap_or_default()
                .to_be_bytes()[..MAX_TABLE_BIT_LENGTH_BYTE]
                .try_into() // this is always success because slice length is MAX_TABLE_BIT_LENGTH_BYTE
                .unwrap_unchecked() // therefore, this is safe
        })
    }
}
impl ops::Deref for TableBits {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tables {
    tbls: Box<[Table]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Table {
    // R0,R1などの名前と値の対応
    tbl: HashMap<Box<str>, TableBits>,
}

impl Table {
    fn from_slice(slc: &[(&str, u64)], length: u8) -> Self {
        Self {
            tbl: slc
                .iter()
                .map(|(k, v)| (k.to_owned().into(), TableBits::from_num(*v as _, length)))
                .collect(),
        }
    }

    pub fn new() -> Self {
        Self {
            tbl: HashMap::new(),
        }
    }

    pub fn add(&mut self, name: Box<str>, val: TableBits) -> Result<(), DoubleDefinitionError> {
        if self.tbl.contains_key(name.as_ref()) {
            Err(DoubleDefinitionError { rule_name: name })
        } else {
            self.tbl.insert(name, val);
            Ok(())
        }
    }
}

impl From<HashMap<Box<str>, TableBits>> for Table {
    fn from(value: HashMap<Box<str>, TableBits>) -> Self {
        Self { tbl: value }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TablesConfig {
    tbls: Vec<Table>,
    names: HashMap<Box<str>, usize>,
}

impl TablesConfig {
    pub fn add_table(&mut self, name: Box<str>, tbl: Table) -> Result<(), tables::Error> {
        if self.names.contains_key(name.as_ref()) {
            Err(DoubleDefinitionError { rule_name: name }.into())
        } else {
            self.names.insert(name, self.tbls.len());
            self.tbls.push(tbl);
            if self.tbls.len() >= 64 {
                Err(tables::Error::TooManyTables)
            } else {
                Ok(())
            }
        }
    }

    pub fn get_index<Q>(&self, name: &Q) -> Option<u8>
    where
        Box<str>: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq + ?Sized,
    {
        self.names.get(name).map(|i| *i as u8)
    }
}

pub mod tables {
    use crate::parser::ErrorKind;

    #[derive(Debug)]
    pub enum Error {
        DoubleDefinitionError(super::DoubleDefinitionError),
        TooManyTables,
    }

    impl From<super::DoubleDefinitionError> for Error {
        fn from(value: super::DoubleDefinitionError) -> Self {
            Self::DoubleDefinitionError(value)
        }
    }

    impl From<Error> for ErrorKind {
        fn from(value: Error) -> Self {
            match value {
                Error::DoubleDefinitionError(e) => {
                    Self::AlreadyExsistentProperty { found: e.rule_name }
                }
                Error::TooManyTables => Self::TooManyTables,
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ImmediateValue(u128);

impl ImmediateValue {
    fn to_le_bytes(mut self, word_size: u8, count: u8) -> [u8; 16] {
        assert!(
            word_size as u16 * count as u16 <= 128,
            "word_size * length must be less than or equal to 128"
        );
        if word_size == 8 {
            return (self.0
                & !(!0u128)
                    .checked_shl((count * word_size) as _)
                    .unwrap_or_default())
            .to_le_bytes();
        } else if word_size == 128 {
            return self.0.to_be_bytes();
        }
        let mut result = 0u128;
        let mask = !(!0u128).checked_shr(word_size as _).unwrap_or_default();
        self.0 <<= 128 - count * word_size;
        for _ in (0..count).rev() {
            result = (result >> word_size) | self.0 & mask;
            self.0 <<= word_size;
        }
        result.to_be_bytes()
    }

    fn to_be_bytes(self, length: u8) -> [u8; 16] {
        assert!(length <= 128, "length must be less than or equal to 128");
        self.0
            .checked_shl(128 - length as u32)
            .unwrap_or_default()
            .to_be_bytes()
    }
}

impl From<u128> for ImmediateValue {
    fn from(value: u128) -> Self {
        Self(value)
    }
}

impl ops::Add<u128> for ImmediateValue {
    type Output = Self;
    fn add(self, rhs: u128) -> Self::Output {
        Self(self.0.wrapping_add(rhs))
    }
}

impl ops::Add for ImmediateValue {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        self + rhs.0
    }
}

impl ops::Sub<u128> for ImmediateValue {
    type Output = Self;
    fn sub(self, rhs: u128) -> Self::Output {
        Self(self.0.wrapping_sub(rhs))
    }
}

impl ops::Sub for ImmediateValue {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self - rhs.0
    }
}

enum Operand {
    Register(Box<str>),
    Immediate(ImmediateValue),
    Label(Box<str>),
}

struct LabelTable {
    tbl: HashMap<Box<str>, ImmediateValue>,
}

pub fn write_bits_slice(slc: &mut [u8], bits: &[u8], pos: usize, len_last: u8) {
    debug_assert!((1..=8).contains(&len_last));
    if bits.is_empty() {
        return;
    }
    let p = pos / 8;
    let q = (pos % 8) as u8;
    let slc = &mut slc[p..];
    let (slc, other) = slc.split_at_mut(bits.len());
    let first = *slc.first().unwrap();
    let last = *slc.last().unwrap();
    use std::iter;
    let bits_aligned = iter::once((first & !(0xff >> q)) | bits[0] >> q).chain(
        bits.windows(2)
            .map(|w| w[0].checked_shl((8 - q) as _).unwrap_or_default() | w[1] >> q),
    );

    bits_aligned.zip(&mut *slc).for_each(|(s, d)| {
        *d = s;
    });
    let a = 8 - q as i8 - len_last as i8;
    use std::cmp::Ordering::*;
    match a.cmp(&0) {
        Greater => {
            let l = slc.last_mut().unwrap();
            *l = (*l & (0xff << a)) | (last & !(0xff << a));
        }
        Less => {
            let l = other.first_mut().unwrap();
            *l = (*l & (0xff >> -a)) | ((*bits.last().unwrap() << (8 - q)) & !(0xff >> -a));
        }
        _ => {}
    }
}

fn write_left_nbits(d: &mut u8, s: u8, n: u8) {
    *d = (*d & (0xff >> n)) | (s & !(0xff >> n));
}

fn write_with_mask(d: &mut u8, s: u8, mask: u8) {
    *d = (*d & !mask) | (s & mask);
}
/// 0 <= `bits_offset` < 8
///
/// 1 <= `len_last` <= 8
///
/// bitsの最初の要素の左から`bits_offset + 1`ビット目からのデータを
/// slcのposビット目からのデータに書き込む
fn write_bits_slice_offset(slc: &mut [u8], bits: &[u8], pos: usize, bits_offset: u8, len_last: u8) {
    debug_assert!(bits_offset < 8, "bits_offset must be less than 8");
    debug_assert!(
        (1..=8).contains(&len_last),
        "len_last must be between 1 and 8"
    );

    if bits.is_empty() || bits.len() == 1 && len_last <= bits_offset {
        return;
    }

    let p = pos / 8;
    let q = (pos % 8) as u8;
    let sh = q as i8 - bits_offset as i8;
    let slc = &mut slc[p..];

    use std::cmp::Ordering::*;
    match sh.cmp(&0) {
        Greater => {
            let first = *slc.first().unwrap();
            write_bits_slice(slc, bits, sh as usize, len_last);
            write_left_nbits(&mut slc[0], first, q);
        }
        Less => {
            if bits.len() == 1 {
                // q - bits_offset < 0 より、bits_offset >= 1だから、
                // nbits = len_last - bits_offset <= 7
                let nbits = len_last - bits_offset;
                let mask = (!(0xffu8 >> nbits)) >> q;
                slc[0] = (slc[0] & !mask) | ((bits[0] << -sh) & mask);
            } else {
                write_with_mask(&mut slc[0], bits[0] << -sh, 0xff >> q);
                write_bits_slice(slc, &bits[1..], (8 + sh) as usize, len_last);
            }
        }
        Equal => {
            let slc = &mut slc[0..bits.len()];
            let f = *slc.first().unwrap();
            let l = *slc.last().unwrap();
            slc.copy_from_slice(bits);
            write_left_nbits(slc.first_mut().unwrap(), f, q);
            write_with_mask(
                slc.last_mut().unwrap(),
                l,
                0xffu8.checked_shr(len_last as _).unwrap_or_default(),
            );
        }
    }
}

#[cfg(test)]
#[test]
fn write_bits_test() {
    use crate::bit_slice::{slice_bit, Bin, Hex, SliceBit};

    fn bin(slc: &[u8]) -> SliceBit<Bin> {
        slice_bit::<Bin>(slc)
    }
    let mut slc = [0u8; 4];
    write_bits_slice(&mut slc, &[0b1010_0000], 0, 4);
    assert_eq!(bin(&slc), bin(&[0b1010_0000, 0, 0, 0]));
    write_bits_slice(&mut slc, &[0b1101_0000], 5, 4);
    assert_eq!(bin(&slc), bin(&[0b1010_0110, 0b1000_0000, 0, 0]));
    write_bits_slice(&mut slc, &[0b1010_1010, 0b1010_1010], 10, 8);
    assert_eq!(
        slice_bit::<Hex>(&slc),
        slice_bit::<Hex>(&[0b1010_0110, 0b1010_1010, 0b1010_1010, 0b1000_0000])
    );

    write_bits_slice_offset(&mut slc, &[0b0000_1111], 13, 4, 8);
    assert_eq!(
        bin(&slc),
        bin(&[0b1010_0110, 0b1010_1111, 0b1010_1010, 0b1000_0000])
    );

    write_bits_slice_offset(&mut slc, &[0b1110_0011], 4, 3, 6);
    assert_eq!(
        bin(&slc),
        bin(&[0b1010_0000, 0b1010_1111, 0b1010_1010, 0b1000_0000])
    );
}

impl Rule {
    fn assemble(
        &self,
        operands: impl Iterator<Item = Option<Operand>>,
        table: &Tables,
        labels: &LabelTable,
        base_address: ImmediateValue,
        general_rule: &GeneralRule,
    ) -> Result<Box<[u8]>, Error> {
        // boolはエンディアンの変更が必要かどうか
        // 数値やアドレス以外の場合はfalse
        // GeneralRuleでリトルエンディアン指定がされている場合はtrue
        use OperandViolation::*;
        let little_endian = general_rule.address_mode & 0b10 != 0;
        let opr_bytes: Vec<Option<Result<(ImmediateValue, bool), TableBits>>> = operands
            .zip(self.operands.iter())
            .map(|(opr, rule)| match (opr, rule) {
                // 検出されたオペランドがテーブルによる指定である場合
                (Some(Operand::Register(r)), OperandRule::Register(key)) => {
                    // 使用可能なテーブルからrを検索し、対応するビット表現を返す
                    if let Some(n) = key
                        .indice()
                        .find_map(|i| table.tbls[i as usize].tbl.get(&r))
                    {
                        Ok(Some(Err(*n)))
                    } else {
                        Err(Error::op_violation(UnknownRegister(r)))
                    }
                }
                // 検出されたオペランドが即値である場合
                (Some(Operand::Immediate(n)), rule) => match rule {
                    OperandRule::Address => Ok(Some(Ok((
                        if self.relative { n - base_address } else { n },
                        little_endian,
                    )))),
                    OperandRule::Register(k) if k.index.get() & 1 == 1 => {
                        Ok(Some(Ok((n, k.is_immediate_only() && little_endian))))
                    }
                    _ => Err(Error::op_violation(CannotUseImmediate)),
                },
                // 検出されたオペランドがラベルである場合
                (Some(Operand::Label(l)), OperandRule::Address) => {
                    // ラベルをラベルテーブルから検索し、ビット表現を取得する
                    if let Some(n) = labels.tbl.get(&l) {
                        Ok(Some(Ok((
                            if self.relative { *n - base_address } else { *n },
                            little_endian,
                        ))))
                    } else {
                        Err(Error::op_violation(UnknownLabel(l)))
                    }
                }
                // テーブルが検出されたがテーブルによる指定不可の場合
                (Some(Operand::Register(_)), _) => Err(Error::op_violation(CannotUseRegister)),
                // ラベルが検出されたがラベルによる指定不可の場合
                (Some(Operand::Label(_)), _) => Err(Error::op_violation(CannotUseLabel)),
                // オペランドが省略された場合
                (None, _) => Ok(None),
            })
            .collect::<Result<_, Error>>()?;
        let mut result: Box<[u8]> = self.code.clone();
        for opr_code in &self.opr_code[..] {
            if let Some(x) = &opr_bytes[opr_code.opr as usize] {
                let pos = opr_code.position;
                let length = opr_code.length;
                let tmp = length % 8;
                let len_last = if tmp == 0 { 8 } else { tmp };
                match x {
                    Ok((v, le)) => {
                        let word_size = general_rule.word_size;
                        write_bits_slice(
                            &mut result,
                            &if !*le || length % word_size != 0 || pos % word_size as u16 != 0 {
                                v.to_be_bytes(length)
                            } else {
                                v.to_le_bytes(word_size, length / word_size)
                                // v.to_le_bytes(word_size, general_rule.address_size_word)
                                // v.to_le_bytes(word_size, general_rule.immediate_size_word)
                            }[..length.div_ceil(8) as usize],
                            pos as usize,
                            len_last,
                        );
                    }
                    Err(v) => write_bits_slice(
                        &mut result,
                        &v[..length.div_ceil(8) as usize],
                        pos as usize,
                        len_last,
                    ),
                }
            }
        }
        Ok(result)
    }
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
                Table::from_slice(
                    &[
                        ("R0", 0),
                        ("R1", 1),
                        ("R2", 2),
                        ("R3", 3),
                        ("R4", 4),
                        ("R5", 5),
                    ],
                    3,
                ),
                Table::from_slice(&[("X0", 0), ("X1", 1), ("X2", 2)], 2),
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
        let general_rule = GeneralRule {
            word_size: 8,
            address_size_word: 2,
            immediate_size_word: 2,
            address_mode: 2,
            empty_symbol_mode: 0,
        };
        let labels = LabelTable {
            tbl: Default::default(),
        };
        let result = rule.assemble(
            [
                Some(Operand::Register("R3".to_owned().into())),
                Some(Operand::Immediate(0x2103.into())),
            ]
            .into_iter(),
            &tables,
            &labels,
            0.into(),
            &general_rule,
        );

        use crate::bit_slice::{separate_into_word, slice_bit, Bin, Hex};
        match result {
            Ok(result) => {
                println!("{:?}", slice_bit::<Bin>(&result));
                println!("{:?}", slice_bit::<Hex>(&result));
                println!("{:?}", result);
                println!("{:?}", separate_into_word(&result, 10));
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    }
}
