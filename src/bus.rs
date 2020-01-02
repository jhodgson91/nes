use num::PrimInt;

pub struct Bus {
    memory: [u8; 64 * 1024],
}

impl Default for Bus {
    fn default() -> Self {
        Bus {
            memory: [0; 64 * 1024],
        }
    }
}

impl Bus {
    pub fn cpu_read<U: PrimInt>(&self, addr: u16) -> U {
        Self::read(addr, &self.memory)
    }

    pub fn cpu_write<U: PrimInt>(&mut self, addr: u16, data: U) {
        Self::write(addr, &mut self.memory, data);
    }

    pub fn write_range(&mut self, addr: u16, data: &[u8]) {
        let addr = addr as usize;
        (&mut self.memory[addr..addr + data.len()]).copy_from_slice(data);
    }

    fn write<U: PrimInt>(addr: u16, to: &mut [u8], data: U) {
        let addr = addr as usize;
        let sl = unsafe {
            // SAFETY - Known size of U guarantees we read and write only size_of<U> bytes
            std::slice::from_raw_parts(
                &data.to_le() as *const U as *const u8,
                std::mem::size_of::<U>(),
            )
        };
        (&mut to[addr..addr + std::mem::size_of::<U>()]).copy_from_slice(&sl);
    }

    fn read<U: PrimInt>(addr: u16, from: &[u8]) -> U {
        let addr = addr as usize;
        let p = &from[addr..addr + std::mem::size_of::<U>()];
        U::from_le(unsafe { std::ptr::read(p.as_ptr() as *const U) })
    }
}
