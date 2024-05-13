use assert_cmd::Command;
use std::error::Error;

#[test]
#[ignore = "reason"]
fn repl() -> Result<(), Box<dyn Error + 'static>> {
    let expected = r#">> Let
Ident("add")
Assign
Function
Lparen
Ident("x")
Comma
Ident("y")
Rparen
Lbrace
Ident("x")
Plus
Ident("y")
Semicolon
Rbrace
Semicolon
>> "#;

    let cmd = Command::cargo_bin("monkey")?
        .write_stdin("let add = fn(x, y) { x + y; };")
        .assert()
        .success();

    let stdout = String::from_utf8(cmd.get_output().stdout.clone())?;
    let parts: Vec<_> = stdout.split('\n').skip(2).collect();
    assert_eq!(expected, parts.join("\n"));

    Ok(())
}
