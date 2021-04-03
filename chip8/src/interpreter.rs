use fake::{Dummy, Fake, Faker};
use rand::Rng;

type Register = usize;
type Address = u16;
type Constant = u8;

const INITIAL_ADDRESS: u16 = 0x200;
const MEMORY_SIZE: usize = 4096;
const SCREEN_SIZE: usize = 64 * 32;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct State {
    registers: [u8; 16],
    memory: [u8; MEMORY_SIZE],
    index: u16,
    pc: u16,
    stack: [u16; 16],
    sp: usize,
    delay_timer: u8,
    sound_timer: u8,
    input_keys: [bool; 16],
    video: [bool; SCREEN_SIZE],
}

impl Dummy<Faker> for State {
    fn dummy_with_rng<R: Rng + ?Sized>(_: &Faker, _rng: &mut R) -> Self {
        let mut memory = [0; MEMORY_SIZE];
        for x in 0..MEMORY_SIZE {
            memory[x] = Faker.fake();
        }
        let mut video = [false; SCREEN_SIZE];
        for x in 0..SCREEN_SIZE {
            video[x] = Faker.fake();
        }
        State {
            registers: Faker.fake(),
            memory,
            index: (0..4096).fake(),
            pc: (INITIAL_ADDRESS..4096).fake(),
            stack: Faker.fake(),
            sp: (1..15).fake(),
            delay_timer: Faker.fake(),
            sound_timer: Faker.fake(),
            input_keys: Faker.fake(),
            video,
        }
    }
}

impl State {
    fn new() -> Self {
        State {
            registers: [0; 16],
            memory: [0; MEMORY_SIZE],
            index: 0,
            pc: INITIAL_ADDRESS,
            stack: [0; 16],
            sp: 0,
            delay_timer: 0,
            sound_timer: 0,
            input_keys: [false; 16],
            video: [false; SCREEN_SIZE],
        }
    }
}

pub enum Operation {
    Clear,
    Return,
    Jump(Address),
    Call(Address),
    SkipEC(Register, Constant),
    SkipNEC(Register, Constant),
    SkipE(Register, Register),
    LoadC(Register, Constant),
    AddC(Register, Constant),
    Load(Register, Register),
    Or(Register, Register),
    And(Register, Register),
    Xor(Register, Register),
    Add(Register, Register),
    SubtractXY(Register, Register),
    ShiftRight(Register),
    SubtractYX(Register, Register),
    ShiftLeft(Register),
    SkipNE(Register, Register),
    LoadIndex(Address),
    JumpRegister(Address),
    Random(Register, Constant),
    Draw(Register, Register, Constant),
    SkipPressed(Register),
    SkipNotPressed(Register),
    LoadFromDelay(Register),
    LoadKeyPress(Register),
    LoadToDelay(Register),
    LoadSound(Register),
    AddIndex(Register),
    LoadFont(Register),
    StoreBinary(Register),
    StoreRegisters(Register),
    ReadRegisters(Register),
}

fn clear(state: &mut State) {
    state.video = [false; SCREEN_SIZE];
}

fn ret(state: &mut State) {
    state.sp -= 1;
    state.pc = state.stack[state.sp];
}

fn jump(state: &mut State, addr: Address) {
    state.pc = addr;
}

fn call(state: &mut State, addr: Address) {
    state.stack[state.sp] = state.pc;
    state.sp += 1;
    state.pc = addr;
}

fn skip_if_equals_constant(state: &mut State, reg: Register, c: Constant) {
    if state.registers[reg] == c {
        state.pc += 2;
    }
}

fn skip_unless_equals_constant(state: &mut State, reg: Register, c: Constant) {
    if state.registers[reg] != c {
        state.pc += 2;
    }
}

fn skip_equals(state: &mut State, reg_x: Register, reg_y: Register) {
    if state.registers[reg_x] == state.registers[reg_y] {
        state.pc += 2;
    }
}

fn load_constant(state: &mut State, reg: Register, c: Constant) {
    state.registers[reg] = c;
}

fn add_constant(state: &mut State, reg: Register, c: Constant) {
    state.registers[reg] = u8::wrapping_add(state.registers[reg], c);
}

fn load(state: &mut State, reg_x: Register, reg_y: Register) {
    state.registers[reg_x] = state.registers[reg_y];
}

fn or(state: &mut State, reg_x: Register, reg_y: Register) {
    state.registers[reg_x] |= state.registers[reg_y];
}

fn and(state: &mut State, reg_x: Register, reg_y: Register) {
    state.registers[reg_x] &= state.registers[reg_y];
}

fn xor(state: &mut State, reg_x: Register, reg_y: Register) {
    state.registers[reg_x] ^= state.registers[reg_y];
}

fn add(state: &mut State, reg_x: Register, reg_y: Register) {
    let result = u8::wrapping_add(state.registers[reg_x], state.registers[reg_y]);
    state.registers[15] = if result < state.registers[reg_x] {
        1
    } else {
        0
    };
    state.registers[reg_x] = result;
}

fn subtract_xy(state: &mut State, reg_x: Register, reg_y: Register) {
    state.registers[15] = if state.registers[reg_x] > state.registers[reg_y] {
        1
    } else {
        0
    };
    state.registers[reg_x] = u8::wrapping_sub(state.registers[reg_x], state.registers[reg_y]);
}

fn shift_right(state: &mut State, reg: Register) {
    state.registers[15] = state.registers[reg] & 1;
    state.registers[reg] >>= 1;
}

fn subtract_yx(state: &mut State, reg_x: Register, reg_y: Register) {
    state.registers[15] = if state.registers[reg_y] > state.registers[reg_x] {
        1
    } else {
        0
    };
    state.registers[reg_x] = u8::wrapping_sub(state.registers[reg_y], state.registers[reg_x]);
}

fn shift_left(state: &mut State, reg: Register) {
    state.registers[15] = (state.registers[reg] & 0b10000000) >> 7;
    state.registers[reg] <<= 1;
}

pub fn exec(state: &mut State, operation: Operation) {
    match operation {
        Operation::Clear => clear(state),
        Operation::Return => ret(state),
        Operation::Jump(addr) => jump(state, addr),
        Operation::Call(addr) => call(state, addr),
        Operation::SkipEC(reg, c) => skip_if_equals_constant(state, reg, c),
        Operation::SkipNEC(reg, c) => skip_unless_equals_constant(state, reg, c),
        Operation::SkipE(reg_x, reg_y) => skip_equals(state, reg_x, reg_y),
        Operation::LoadC(reg, c) => load_constant(state, reg, c),
        Operation::AddC(reg, c) => add_constant(state, reg, c),
        Operation::Load(reg_x, reg_y) => load(state, reg_x, reg_y),
        Operation::Or(reg_x, reg_y) => or(state, reg_x, reg_y),
        Operation::And(reg_x, reg_y) => and(state, reg_x, reg_y),
        Operation::Xor(reg_x, reg_y) => xor(state, reg_x, reg_y),
        Operation::Add(reg_x, reg_y) => add(state, reg_x, reg_y),
        Operation::SubtractXY(reg_x, reg_y) => subtract_xy(state, reg_x, reg_y),
        Operation::ShiftRight(reg) => shift_right(state, reg),
        Operation::SubtractYX(reg_x, reg_y) => subtract_yx(state, reg_x, reg_y),
        Operation::ShiftLeft(reg) => shift_left(state, reg),
        _ => panic!("Unimplemented!"),
    }
}

#[cfg(test)]
mod tests {
    use super::Operation::*;
    use super::*;
    use fake::{Fake, Faker};

    #[test]
    fn test_clear() {
        let mut state = Faker.fake::<State>();
        exec(&mut state, Clear);
        let mut result = false;
        for x in 0..SCREEN_SIZE {
            result = result || state.video[x];
        }
        assert_eq!(result, false);
    }

    #[test]
    fn test_ret() {
        let mut state = Faker.fake::<State>();
        let sp = state.sp;
        exec(&mut state, Return);
        assert_eq!(sp - 1, state.sp);
        assert_eq!(state.pc, state.stack[state.sp]);
    }

    #[test]
    fn test_jump() {
        let mut state = Faker.fake::<State>();
        let addr = Faker.fake();
        exec(&mut state, Jump(addr));
        assert_eq!(addr, state.pc);
    }

    #[test]
    fn test_call() {
        let mut state = Faker.fake::<State>();
        let addr = Faker.fake();
        let pc = state.pc;
        exec(&mut state, Call(addr));
        assert_eq!(state.pc, addr);
        assert_eq!(state.stack[state.sp - 1], pc);
    }

    #[test]
    fn test_call_and_return() {
        let mut state = Faker.fake::<State>();
        let pc = state.pc;
        exec(&mut state, Call(Faker.fake()));
        exec(&mut state, Return);
        assert_eq!(pc, state.pc);
    }

    #[test]
    fn test_skip_if_equals_constant() {
        let mut state = Faker.fake::<State>();
        let pc = state.pc;
        let register = (0..16).fake();
        let register_val = (0..0xff).fake();
        state.registers[register] = register_val;
        exec(&mut state, SkipEC(register, register_val + 1));
        assert_eq!(pc, state.pc);
        exec(&mut state, SkipEC(register, register_val));
        assert_eq!(pc + 2, state.pc);
    }

    #[test]
    fn test_skip_unless_equals_constant() {
        let mut state = Faker.fake::<State>();
        let pc = state.pc;
        let register = (0..16).fake();
        let register_val = (0..0xff).fake();
        state.registers[register] = register_val;
        exec(&mut state, SkipNEC(register, register_val));
        assert_eq!(pc, state.pc);
        exec(&mut state, SkipNEC(register, register_val + 1));
        assert_eq!(pc + 2, state.pc);
    }

    #[test]
    fn test_skip_equals() {
        let mut state = Faker.fake::<State>();
        let pc = state.pc;
        let register_x = (0..8).fake();
        let register_y = (8..16).fake();
        let register_val = (0..0xff).fake();
        state.registers[register_x] = register_val;
        state.registers[register_y] = register_val + 1;
        exec(&mut state, SkipE(register_x, register_y));
        assert_eq!(pc, state.pc);
        state.registers[register_y] = register_val;
        exec(&mut state, SkipE(register_x, register_y));
        assert_eq!(pc + 2, state.pc);
    }

    #[test]
    fn test_load_constant() {
        let mut state = Faker.fake::<State>();
        let register = (0..16).fake::<usize>();
        let fake_val = (0..0xff).fake();
        state.registers[register] = fake_val;
        exec(&mut state, LoadC(register, fake_val + 1));
        assert_eq!(state.registers[register], fake_val + 1);
    }

    #[test]
    fn test_add_constant() {
        let mut state = Faker.fake::<State>();
        let register = (0..16).fake::<usize>();
        let constant = Faker.fake();
        let register_val = state.registers[register];
        exec(&mut state, AddC(register, constant));
        assert_eq!(
            state.registers[register],
            u8::wrapping_add(register_val, constant)
        );
    }

    #[test]
    fn test_load() {
        let mut state = Faker.fake::<State>();
        let register_x = (0..8).fake::<usize>();
        let register_y = (8..16).fake::<usize>();
        exec(&mut state, Load(register_x, register_y));
        assert_eq!(state.registers[register_x], state.registers[register_y]);
    }

    #[test]
    fn test_or() {
        let mut state = Faker.fake::<State>();
        let register_x = (0..15).fake::<usize>();
        let register_y = (0..16).fake::<usize>();
        let register_z = register_x + 1;

        let mut register_val = state.registers[register_x];
        exec(&mut state, Or(register_x, register_y));
        assert_eq!(
            state.registers[register_x],
            register_val | state.registers[register_y]
        );

        state.registers[register_z] = 0;
        register_val = state.registers[register_x];
        exec(&mut state, Or(register_x, register_z));
        assert_eq!(state.registers[register_x], register_val);

        register_val = state.registers[register_x];
        exec(&mut state, Or(register_x, register_x));
        assert_eq!(state.registers[register_x], register_val);

        state.registers[register_z] = 0xff;
        exec(&mut state, Or(register_x, register_z));
        assert_eq!(state.registers[register_x], 0xff);
    }

    #[test]
    fn test_and() {
        let mut state = Faker.fake::<State>();
        let register_x = (0..15).fake::<usize>();
        let register_y = (0..16).fake::<usize>();
        let register_z = register_x + 1;

        let mut register_val = state.registers[register_x];
        exec(&mut state, And(register_x, register_y));
        assert_eq!(
            state.registers[register_x],
            register_val & state.registers[register_y]
        );

        register_val = state.registers[register_x];
        state.registers[register_z] = 0xff;
        exec(&mut state, And(register_x, register_z));
        assert_eq!(state.registers[register_x], register_val);

        register_val = state.registers[register_x];
        exec(&mut state, And(register_x, register_x));
        assert_eq!(state.registers[register_x], register_val);

        state.registers[register_z] = 0;
        exec(&mut state, And(register_x, register_z));
        assert_eq!(state.registers[register_x], 0);
    }

    #[test]
    fn test_xor() {
        let mut state = Faker.fake::<State>();
        let register_x = (0..14).fake::<usize>();
        let register_y = register_x + 1;
        let register_z = register_x + 2;

        let mut register_val = state.registers[register_x];
        exec(&mut state, Xor(register_x, register_y));
        assert_eq!(
            state.registers[register_x],
            register_val ^ state.registers[register_y]
        );

        register_val = state.registers[register_x];
        state.registers[register_z] = 0;
        exec(&mut state, Xor(register_x, register_z));
        assert_eq!(state.registers[register_x], register_val);

        exec(&mut state, Xor(register_x, register_x));
        assert_eq!(state.registers[register_x], 0);
    }

    #[test]
    fn test_add() {
        let mut state = Faker.fake::<State>();
        let register_x = (0..8).fake::<usize>();
        let register_y = (8..15).fake::<usize>();

        state.registers[register_x] = 0xff;
        state.registers[register_y] = 1;
        exec(&mut state, Add(register_x, register_y));
        assert_eq!(state.registers[register_x], 0);
        assert_eq!(state.registers[15], 1);

        state.registers[register_x] = 0xfe;
        state.registers[register_y] = 1;
        exec(&mut state, Add(register_x, register_y));
        assert_eq!(state.registers[register_x], 0xff);
        assert_eq!(state.registers[15], 0);
    }

    #[test]
    fn test_subtract_xy() {
        let mut state = Faker.fake::<State>();
        let register_x = (0..8).fake::<usize>();
        let register_y = (8..15).fake::<usize>();

        state.registers[register_x] = 0;
        state.registers[register_y] = 1;
        exec(&mut state, SubtractXY(register_x, register_y));
        assert_eq!(state.registers[register_x], 0xff);
        assert_eq!(state.registers[15], 0);

        state.registers[register_x] = 2;
        state.registers[register_y] = 1;
        exec(&mut state, SubtractXY(register_x, register_y));
        assert_eq!(state.registers[register_x], 1);
        assert_eq!(state.registers[15], 1);
    }

    #[test]
    fn test_shift_right() {
        let mut state = Faker.fake::<State>();
        let register = (0..15).fake::<usize>();

        let register_val = state.registers[register];
        exec(&mut state, ShiftRight(register));
        assert_eq!(state.registers[15], register_val & 1);
        assert_eq!(state.registers[register], register_val >> 1);
    }

    #[test]
    fn test_subtract_yx() {
        let mut state = Faker.fake::<State>();
        let register_x = (0..8).fake::<usize>();
        let register_y = (8..15).fake::<usize>();

        state.registers[register_x] = 1;
        state.registers[register_y] = 0;
        exec(&mut state, SubtractYX(register_x, register_y));
        assert_eq!(state.registers[register_x], 0xff);
        assert_eq!(state.registers[15], 0);

        state.registers[register_x] = 1;
        state.registers[register_y] = 2;
        exec(&mut state, SubtractYX(register_x, register_y));
        assert_eq!(state.registers[register_x], 1);
        assert_eq!(state.registers[15], 1);
    }

    #[test]
    fn test_shift_left() {
        let mut state = Faker.fake::<State>();
        let register = (0..15).fake::<usize>();

        let register_val = state.registers[register];
        exec(&mut state, ShiftLeft(register));
        assert_eq!(state.registers[15], (register_val & 0b10000000) >> 7);
        assert_eq!(state.registers[register], register_val << 1);
    }
}
