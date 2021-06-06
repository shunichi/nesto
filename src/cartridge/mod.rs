use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub struct Cartridge {
    pub mapper: u32,
    pub prg: std::vec::Vec<u8>,
    pub chr: std::vec::Vec<u8>,
}

#[derive(Debug, Clone)]
struct CartridgeError;

impl fmt::Display for CartridgeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid cartridge")
    }
}
impl std::error::Error for CartridgeError {}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const SIGNATURE: [u8; 4] = [78, 69, 83, 26];

pub fn read(path: &Path) -> Result<Cartridge> {
    let mut f = File::open(path)?;
    let mut buffer = [0; 16];

    f.read(&mut buffer)?;
    println!("{:?}", &buffer);
    if buffer[0..4] != SIGNATURE {
        Err(CartridgeError.into())
    } else {
        Ok(Cartridge {
            mapper: 0,
            prg: vec![0],
            chr: vec![0],
        })
    }
}
