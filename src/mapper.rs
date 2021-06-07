use crate::cartridge::Cartridge;

pub trait Mapper {
    fn read(&self, cartridge: &Cartridge, address: u16) -> u8;
    fn read16(&self, cartridge: &Cartridge, address: u16) -> u16 {
        (self.read(&cartridge, address + 1) as u16) << 8 | self.read(&cartridge, address) as u16
    }
}

pub struct Mapper0 {}

impl Mapper0 {
    pub fn new() -> Self {
        Mapper0 {}
    }
}
impl Mapper for Mapper0 {
    fn read(&self, cartridge: &Cartridge, address: u16) -> u8 {
        if address >= 0x8000 {
            let mut index: usize = address as usize - 0x8000;
            if cartridge.prg.len() == 0x4000 {
                index = index % 0x4000;
            }
            return cartridge.prg[index];
        }
        return 0;
    }
}
