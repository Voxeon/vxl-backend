use crate::parser::ast::AST;
use super::builder::Builder;
use super::context::Context;
use crate::error::VoxlError;
use rand::Rng;
use sha2::{Digest, Sha256};
use std::error::Error;
use std::fs::OpenOptions;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::process::Output;
use std::{fmt, io::Write};

pub struct Compiler<E: Error, B: Builder<ErrorType = E>> {
    assembler_path: String,
    assembler_arguments: Vec<String>,
    builder: B,
}

impl<E: Error, B: Builder<ErrorType = E>> Compiler<E, B> {
    const DEFAULT_ASSEMBLY_DIRECTORY: &'static str = "/tmp";

    pub fn new(assembler_path: String, assembler_arguments: Vec<String>, builder: B) -> Self {
        return Self {
            assembler_path,
            assembler_arguments,
            builder,
        };
    }

    pub fn compile_ast<O: AsRef<Path>>(
        &mut self,
        ast: AST,
        output_path: O,
    ) -> Result<(), VoxlError<E>> {
        let mut buffer = PathBuf::from(Self::DEFAULT_ASSEMBLY_DIRECTORY);

        match output_path.as_ref().file_stem() {
            Some(str) => {
                let mut hasher = Sha256::new();
                hasher.update(str.to_str().unwrap());
                let result = format!("{}.asm", hex::encode(&hasher.finalize()[0..64]));
                buffer.push(result);
            }
            None => {
                let mut rng = rand::thread_rng();
                let left: [u8; 32] = rng.gen();
                let right: [u8; 32] = rng.gen();
                let result = format!("{}{}.asm", hex::encode(left), hex::encode(right));
                buffer.push(result);
            }
        }

        return self.compile_ast_with_temp(ast, buffer, output_path.as_ref().to_str().unwrap());
    }

    pub fn compile_ast_with_temp<T: AsRef<Path>>(
        &mut self,
        ast: AST,
        temp_path: T,
        output_path: &str,
    ) -> Result<(), VoxlError<E>> {
        let contexts = self.convert_ast_to_operations(ast)?;

        for context in contexts {
            self.builder
                .insert_context(context)
                .map_err(|e| VoxlError::BuilderError(e))?;
        }

        let output: Vec<u8> = self
            .builder
            .build()
            .map_err(|e| VoxlError::BuilderError(e))?
            .into();

        let mut f = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(temp_path.as_ref())
            .map_err(|e| VoxlError::new_io_error(e))?;

        f.write_all(&output)
            .map_err(|e| VoxlError::new_io_error(e))?;

        f.flush().map_err(|e| VoxlError::new_io_error(e))?;

        let result = Command::new(self.assembler_path.clone())
            .args(&self.assembler_arguments)
            .arg("-o")
            .arg(output_path)
            .output()
            .map_err(|e| VoxlError::AssemblerError(format!("{}", e)))?;

        std::fs::remove_file(temp_path.as_ref()).map_err(|e| VoxlError::new_io_error(e))?;

        if !result.status.success() {
            let code = result.status.code();

            let mut err = match code {
                Some(code) => format!("{} exited with error code {}\n", self.assembler_path, code,),
                None => format!("{} exited with an unknown error.\n", self.assembler_path),
            };

            if let Ok(s) = std::str::from_utf8(&result.stdout) {
                err.push_str(&format!("Assembler stdout: {}\n", s));
            }

            if let Ok(s) = std::str::from_utf8(&result.stderr) {
                err.push_str(&format!("Assembler stderr: {}\n", s));
            }

            return Err(VoxlError::AssemblerError(err));
        }

        return Ok(());
    }

    fn convert_ast_to_operations(
        &self,
        ast: AST,
    ) -> Result<Vec<Box<dyn Context<ErrorType = E>>>, VoxlError<E>> {
        todo!();
    }
}
