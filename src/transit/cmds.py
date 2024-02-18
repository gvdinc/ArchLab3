from __future__ import annotations

from enum import Enum


class Cmd(Enum):
    NOP: str = "nop"
    HALT: str = "halt"
    LDM: str = "ldm"
    LDI: str = "ldi"
    SAVE: str = "save"
    ADD: str = "add"
    SUB: str = "sub"
    CMP: str = "cmp"
    INCR: str = "incr"
    DECR: str = "decr"
    LSL: str = "lsl"
    LSR: str = "lsr"
    CLRC: str = "clrc"
    CLRZ: str = "clrz"
    AND: str = "and"
    OR: str = "or"
    NOT: str = "not"
    XOR: str = "xor"
    JMP: str = "jmp"
    JZ: str = "jz"
    JZC: str = "jzc"
    MULT: str = "mult"
    DIV: str = "dev"
    MOD: str = "mod"
    POW: str = "pow"
    SQRT: str = "sqrt"
    IN: str = "in"
    OUT: str = "out"
    LDREF: str = "ldref"
    SAVEREF: str = "saveref"


class Command(Enum):
    NOP: int = 0
    HALT: int = 1
    LDM: int = 2
    LDI: int = 3
    SAVE: int = 4
    ADD: int = 5
    SUB: int = 6
    CMP: int = 7
    INCR: int = 8
    DECR: int = 9
    LSL: int = 10
    LSR: int = 11
    CLRC: int = 12
    CLRZ: int = 13
    AND: int = 14
    OR: int = 15
    NOT: int = 16
    XOR: int = 17
    JMP: int = 18
    JZ: int = 19
    JZC: int = 20
    MULT: int = 21
    DIV: int = 22
    MOD: int = 23
    POW: int = 24
    SQRT: int = 25
    IN: int = 26
    OUT: int = 27
    LDREF: int = 28
    SAVEREF: int = 29


cmd_codes: dict[Cmd, str] = {
    Cmd.NOP: bin(0).zfill(16).replace("0b", ""),  # Операция простоя
    Cmd.HALT: bin(1).zfill(16).replace("0b", ""),  # Сигнал остановки
    Cmd.LDM: bin(2).zfill(16).replace("0b", ""),  # Загрузить знач из памяти данных в acc
    Cmd.LDI: bin(3).zfill(16).replace("0b", ""),  # Загрузить знач из памяти команд в acc
    Cmd.SAVE: bin(4).zfill(16).replace("0b", ""),  # Выгрузить значение аккумулятора в ячейку памяти
    Cmd.ADD: bin(5).zfill(16).replace("0b", ""),  # Добавить значение из ячейки к аккумулятору
    Cmd.SUB: bin(6).zfill(16).replace("0b", ""),  # Вычесть значение ячейки из аккумулятора
    Cmd.CMP: bin(7).zfill(16).replace("0b", ""),  # Сравнить код из аккумулятора c ячейкой памяти
    Cmd.INCR: bin(8).zfill(16).replace("0b", ""),  # Инкрементирует значение аккумулятора
    Cmd.DECR: bin(9).zfill(16).replace("0b", ""),  # Декрементирует значение аккумулятора
    Cmd.LSL: bin(10).zfill(16).replace("0b", ""),  # Бит. Сдвиг влево, устанавливается флаг c
    Cmd.LSR: bin(11).zfill(16).replace("0b", ""),  # Бит. Сдвиг вправо, устанавливается флаг c
    Cmd.CLRC: bin(12).zfill(16).replace("0b", ""),  # Сбрасывает значение флага переполнения
    Cmd.CLRZ: bin(13).zfill(16).replace("0b", ""),  # Сбрасывает значение нулевого флага
    Cmd.AND: bin(14).zfill(16).replace("0b", ""),  # Логическое и acc
    Cmd.OR: bin(15).zfill(16).replace("0b", ""),  # Логическое или
    Cmd.NOT: bin(16).zfill(16).replace("0b", ""),  # Логическое не acc
    Cmd.XOR: bin(17).zfill(16).replace("0b", ""),  # Логический xor acc
    Cmd.JMP: bin(18).zfill(16).replace("0b", ""),  # Совершить переход на pmem
    Cmd.JZ: bin(19).zfill(16).replace("0b", ""),  # Переход на pmem при установленном zero флаге 1
    Cmd.JZC: bin(20).zfill(16).replace("0b", ""),  # Переход на pmem при неустановленном zero флаге
    Cmd.MULT: bin(21).zfill(16).replace("0b", ""),  # Умножить аккумулятор на знач. Из ячейки пам.
    Cmd.DIV: bin(22).zfill(16).replace("0b", ""),  # Разделить аккумулятор на знач. Из ячейки пам.
    Cmd.MOD: bin(23).zfill(16).replace("0b", ""),  # Остаток от деления аккумулятора на знач. Ячейки
    Cmd.POW: bin(24).zfill(16).replace("0b", ""),  # Возвести аккумулятор в степень из ячейки пам.
    Cmd.SQRT: bin(25).zfill(16).replace("0b", ""),  # Подсчитать корень знач. Аккумулятора
    Cmd.IN: bin(26).zfill(16).replace("0b", ""),  # Ввод символьного значения с внешнего устройства
    Cmd.OUT: bin(27).zfill(16).replace("0b", ""),  # Вывод символьного значения на внешнее устройство
    Cmd.LDREF: bin(28).zfill(16).replace("0b", ""),  # Относительная загрузка из памяти
    Cmd.SAVEREF: bin(29).zfill(16).replace("0b", ""),  # Относительное сохранение в память
}

representation: list[str] = [
    "nop",  # 0
    "halt",  # 1
    "ldm",  # 2
    "ldi",  # 3
    "save",  # 4
    "add",  # 5
    "sub",  # 6
    "cmp",  # 7
    "incr",  # 8
    "decr",  # 9
    "lsl",  # 10
    "lsr",  # 11
    "clrc",  # 12
    "clrz",  # 13
    "and",  # 14
    "or",  # 15
    "not",  # 16
    "xor",  # 17
    "jmp",  # 18
    "jz",  # 19
    "jzc",  # 20
    "mult",  # 21
    "dev",  # 22
    "mod",  # 23
    "pow",  # 24
    "sqrt",  # 25
    "in",  # 26
    "out",  # 27
    "ldref",  # 28
    "saveref"  # 29
]
