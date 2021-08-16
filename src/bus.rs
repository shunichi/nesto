use crate::cartridge::Cartridge;
use crate::mapper::Mapper;
use std::boxed::Box;
use std::vec::Vec;

pub struct Bus {
    pub mapper: Box<dyn Mapper>,
    ram: Vec<u8>,
}

impl Bus {
    const RAM_SIZE: usize = 2 * 1024;

    pub fn new(mapper: Box<dyn Mapper>) -> Self {
        Bus {
            mapper,
            ram: vec![0; Self::RAM_SIZE],
        }
    }

    pub fn cartridge(&self) -> &Cartridge {
        self.mapper.cartridge()
    }

    pub fn read(&self, addr: u16) -> u8 {
        if addr < 0x2000 {
            self.ram[(addr & 0x7ff) as usize]
        } else if addr >= 0x8000 {
            self.mapper.read(addr)
        } else if 0x4000 <= addr && addr <= 0x4017 {
            // TODO: Implement APU
            0xff
        } else {
            0
        }
    }

    pub fn read16(&self, addr: u16) -> u16 {
        let lo: u16 = self.read(addr) as u16;
        let hi: u16 = self.read((addr as u32 + 1) as u16) as u16;
        hi << 8 | lo
    }

    pub fn read16zero(&self, addr: u16) -> u16 {
        let addr = addr & 0xff;
        let lo: u16 = self.read(addr) as u16;
        let hi: u16 = self.read((addr + 1) & 0xff) as u16;
        hi << 8 | lo
    }

    pub fn read16bug(&self, addr: u16) -> u16 {
        use std::num::Wrapping;
        let lo: u16 = self.read(addr) as u16;
        let hi_addr = (addr & 0xff00) + (Wrapping(addr as u8) + Wrapping(1u8)).0 as u16;
        // let hi_addr = (addr & 0xff00) + ((addr as u32 + 1) & 0xff) as u16;
        let hi: u16 = self.read(hi_addr) as u16;
        hi << 8 | lo
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        if addr < 0x2000 {
            self.ram[(addr & 0x7ff) as usize] = value;
        }
    }
}
