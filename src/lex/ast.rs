//! 構文木の生成

/// 構文木のノードの一般化
// `T: ?Sized`であるが、フィールドがprivateであり、`T: ?Sized`であるときに使えるメソッドが定義されていないため、現時点では意味がない。
// メタアセンブラにおいては使われない可能性が高い
pub struct Node<T: ?Sized, const OPR: usize> {
    children: [Option<Box<Node<T, OPR>>>; OPR],
    token: T,
}

impl<T, const OPR: usize> Node<T, OPR> {
    pub fn new(token: T) -> Self {
        Self {
            children: std::array::from_fn(|_| None),
            token,
        }
    }
}
