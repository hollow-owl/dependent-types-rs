use std::collections::HashMap;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result, Config};

use crate::CoC::Term;

pub mod CoC;
fn main() -> Result<()> {
    let config = Config::builder().auto_add_history(true).build();
    let mut rl = DefaultEditor::with_config(config)?;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let term = Term::parse(&line).unwrap();
                // let ty = term.infer_type(&HashMap::new());
                let evaled = term.eval();
                // let evaled_ty = evaled.infer_type(&HashMap::new());
                // println!("ty: {ty:?}\nevaled: {evaled:?}\nevaled_ty: {evaled_ty:?}");
                println!("evaled: {evaled:?}");
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    Ok(())
}