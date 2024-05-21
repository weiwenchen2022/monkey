use super::Object;
use fnv::FnvBuildHasher;
use std::rc::Rc;

#[test]
fn string_hashkey() {
    let hello1: Object = "Hello World".into();
    let hello2: Object = "Hello World".into();
    let diff1: Object = "My name is johnny".into();
    let diff2: Object = "My name is johnny".into();

    let builder = FnvBuildHasher::default();
    assert_eq!(
        compute_hash(&builder, &hello1),
        compute_hash(&builder, &hello2),
    );

    assert_eq!(
        compute_hash(&builder, &diff1),
        compute_hash(&builder, &diff2),
    );

    assert_ne!(
        compute_hash(&builder, &hello1),
        compute_hash(&builder, &diff1),
    )
}

#[test]
fn boolean_hashkey() {
    let true1 = Object::Boolean(true);
    let true2 = Object::Boolean(true);
    let false1 = Object::Boolean(false);
    let false2 = Object::Boolean(false);

    let builder = FnvBuildHasher::default();
    assert_eq!(
        compute_hash(&builder, &true1),
        compute_hash(&builder, &true2),
    );

    assert_eq!(
        compute_hash(&builder, &false1),
        compute_hash(&builder, &false2),
    );

    assert_ne!(
        compute_hash(&builder, &true1),
        compute_hash(&builder, &false1),
    )
}

#[test]
fn integer_hashkey() {
    let one1 = Object::Integer(1);
    let one2 = Object::Integer(1);
    let two1 = Object::Integer(2);
    let two2 = Object::Integer(2);

    let builder = FnvBuildHasher::default();
    assert_eq!(compute_hash(&builder, &one1), compute_hash(&builder, &one2),);

    assert_eq!(compute_hash(&builder, &two1), compute_hash(&builder, &two2),);

    assert_ne!(compute_hash(&builder, &one1), compute_hash(&builder, &two1),)
}

use std::hash::{BuildHasher, Hash, Hasher};

fn compute_hash<B: BuildHasher, T: Hash>(builder: &B, value: &T) -> u64 {
    let mut hasher = builder.build_hasher();
    value.hash(&mut hasher);
    hasher.finish()
}
