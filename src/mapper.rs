use crate::cartridge::Cartridge;

pub trait Mapper {
    fn read(&self, address: u16) -> u8;
    fn read16(&self, address: u16) -> u16 {
        (self.read(address + 1) as u16) << 8 | self.read(address) as u16
    }
    fn cartridge(&self) -> &Cartridge;
}

pub struct Mapper0 {
    pub cartridge: Cartridge,
}

impl Mapper0 {
    pub fn new(cartridge: Cartridge) -> Self {
        Mapper0 { cartridge }
    }
}
impl Mapper for Mapper0 {
    fn read(&self, address: u16) -> u8 {
        if address >= 0x8000 {
            let mut index: usize = address as usize - 0x8000;
            if self.cartridge.prg.len() == 0x4000 {
                index = index % 0x4000;
            }
            return self.cartridge.prg[index];
        }
        return 0;
    }
    fn cartridge(&self) -> &Cartridge {
        &self.cartridge
    }
}
