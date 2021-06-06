use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub struct Cartridge {
    pub mapper: u8,
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

// http://wiki.nesdev.com/w/index.php/INES
//
// 0-3: Constant $4E $45 $53 $1A ("NES" followed by MS-DOS end-of-file)
// 4: Size of PRG ROM in 16 KB units
// 5: Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)
// 6: Flags 6 - Mapper, mirroring, battery, trainer
// 7: Flags 7 - Mapper, VS/Playchoice, NES 2.0
// 8: Flags 8 - PRG-RAM size (rarely used extension)
// 9: Flags 9 - TV system (rarely used extension)
// 10: Flags 10 - TV system, PRG-RAM presence (unofficial, rarely used extension)
// 11-15: Unused padding (should be filled with zero, but some rippers put their name across bytes 7-15)

const SIGNATURE: [u8; 4] = [78, 69, 83, 26];

pub fn read(path: &Path) -> Result<Cartridge> {
    let mut f = File::open(path)?;
    let mut header = [0; 16];

    f.read_exact(&mut header)?;

    if header[0..4] != SIGNATURE {
        return Err(CartridgeError.into());
    }
    if (header[6] & (1u8 << 2)) != 0 {
        // trainer
    }
    let mapper = header[7] | (header[6] >> 4);

    let prg_bytes = (header[4] as usize) * 16 * 1024;
    let chr_bytes = (header[5] as usize) * 8 * 1024;
    let mut prg = vec![0u8; prg_bytes];
    let mut chr = vec![0u8; chr_bytes];
    f.read_exact(prg.as_mut_slice())?;
    f.read_exact(chr.as_mut_slice())?;

    Ok(Cartridge {
        mapper: mapper,
        prg: prg,
        chr: chr,
    })
}
