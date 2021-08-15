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
        } else {
            0
        }
    }

    pub fn read16(&self, addr: u16) -> u16 {
        (self.read(((addr as u32 + 1) & 0xffff) as u16) as u16) << 8 | (self.read(addr) as u16)
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        if addr < 0x2000 {
            self.ram[(addr & 0x7ff) as usize] = value;
        }
    }
}
