# Совкод (.ussr) Транслятор и модель

- `alg | acc | harv | hw | instr | binary | stream | port | pstr | prob1 | 8bit`
- Без усложнения

Задание к лабораторной [lab3-task.md](lab3-task.md).
Персональный вариант в файле [variant.md](variant.md).

## Язык программирования

Синтаксис языка должен напоминать java/javascript/lua. Должен поддерживать математические выражения.

- необходимо объяснить, как происходит отображение больших выражений на регистры и память;
- необходимо продемонстрировать работу транслятора в случае, если количество регистров недостаточно для реализации
  алгоритма.

Язык назовём Совкод (`☭★Cоветский код★☭`)

### Типы данных

- цел - целочисленный (int)
- вещ - вещественный (float тип)
- симв - символьный (char тип)
- строка (указатель на строку в памяти)
  того 6 типов данных

### Операции

- `+ - / *` (классические операции сложения, вычитания, деления и умножения соотв.)
- `% ^ ~` мат. операции остаток от деления, возведение в степень и взятие корня.
- `!` логическое отрицание
- `&` побитовая конъюнкция
- `|` побитовая дизъюнкция
- `< > ==` отношения (меньше, больше и равно соответственно)
- `(переменная)++` `(переменная)--` операции инкремент и декремент соотв.
- `\>> (переменная)` чтение из потока ввода 1 символа
- `\<< (переменная)` запись в файл вывода 1 символа

### Выражения

#### `Если` Оператор условия (if)

* Оператор если выполняет код внутри блока {} при истинности логического выражения в скобках ().
* В случае ложности выражения, код в блоке иначе выполняется (если он присутствует).
* Пример:

> `если` _(...)_ { кол_во++; }  
> `иначе` { кол-во--; }

#### `Пока` - оператор цикла (while)

Оператор пока выполняет код внутри блока {} до тех пор, пока логическое выражение в скобках () истинно.
Пример:
> `пока` _((кол_во > 12) & (a + b == 12))_ {...}

`/*` _текст комментария_ `*/` - комментарии

#### Логическое выражение

Логические выражения могут содержать сравнения, логические операторы (&, |, !), и могут быть вложены в скобки.
Пример:
> ((кол_во > 12) & (a + b == 12))

#### Комментарии:

* Комментарии начинаются с символа # и распространяются до конца строки.
* Пример:

> цел а = 5; `# Это комментарий`

### Формальное описание синтаксиса

<details><summary>Формальное описание Совкода по форме Бэкуса-Наура</summary>

``` ebnf
    <Совкод> ::= { <Объявление> | <Оператор> | <Комментарий> }
    
    <Объявление> ::= <Тип> <Идентификатор> {, <Идентификатор>} ;
    
    <Тип> ::= "цел" | "вещ" | "симв" | "строка"
    
    <Идентификатор> ::= <Буква> {<Буква> | <Цифра>}
    
    <Буква> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z" | "_"
    
    <Цифра> ::= "0" | "1" | ... | "9"
    
    <Оператор> ::= "если" "(" <Логическое_выражение> ")" "{" <Код> "}" ["иначе" "{" <Код> "}"]
     | "пока" "(" <Логическое_выражение> ")" "{" <Код> "}"
     | "#" <Текст_комментария>
     | <Выражение> ";"
    
    <Логическое_выражение> ::= <Сравнение> | <Логическое_выражение> "&" <Логическое_выражение>
     | <Логическое_выражение> "|" <Логическое_выражение>
     | "!" <Логическое_выражение>
    
    <Сравнение> ::= <Арифметическое_выражение> ("<" | ">" | "==") <Арифметическое_выражение>
    
    <Арифметическое_выражение> ::= <Терм> {"+" | "-" <Терм>}
    
    <Терм> ::= <Фактор> {"*" | "/" <Фактор>}
    
    <Фактор> ::= <Операнд> {"%" | "^" | "~" }
    
    <Операнд> ::= "(" <Логическое_выражение> ")"
     | <Идентификатор>
     | <Константа>
     | "\" ">>" "(" <Идентификатор> ")"
     | "\" "<<" "(" <Идентификатор> ")"
    
    <Константа> ::= <Целое> | <Вещественное> | <Логическое> | <Символьное>
    
    <Целое> ::= <Цифра> {<Цифра>}
    
    <Вещественное> ::= <Целое> "." <Целое>
    
    <Символьное> ::= "'" <Символ> "'"
    
    <Символ> ::= <Буква> | <Цифра> | <Спецсимвол>
    
    <Спецсимвол> ::= "#", "$"
    
    <Код> ::= { <Объявление> | <Оператор> }
    
    <Комментарий> ::= "#" <Текст_комментария>
    
    <Текст_комментария> ::= <Любой_текст_комментария>
    
    <Любой_текст_комментария> ::= {<Любой_символ>}
    
    <Любой_символ> ::= <Буква> | <Цифра>
    
     <
     Спецсимвол> | " " | "," | "." | ";" | "(" | ")" | "{" | "}" | "'" | "\" | "=" | "!" | "&" | "|" | "<" | ">" | "+" | "-" | "*" | "/" | "%" | "^" | "~"
     ... Как-то так
```

</details>

Таким образом, семантика ☭★Совкода★☭ определяет порядок выполнения кода, правила объявления переменных,
структуру условных операторов и циклов, а также различные операции, которые могут быть использованы в программах на этом
языке.

**## Транслятор Совкода _Stalin_

`stalin <путь до исходника .ussr> <имя бинарника>`

### Входные данные

* Имя файла с исходным кодом в текстовом виде.
* Имя файла для сохранения полученного машинного кода.

### Выходные данные

* В поток ошибок записывается ассемблерное представление кода
* В поток ошибок записывается память данных
* В поток ошибок записываются количества строк кода в реализации алгоритма, количество инструкций и количество байтов в
  бинарном файле в целом
* В файл записывает байткод программы

### Основные этапы компиляции

* Трансформирование текста в последовательность значимых термов. [Lexer.py](src/translator/s_lexer.py)
* Парсинг программы. [Parser.py](src/translator/s_parser.py)
* Генерация машинного кода в случае, если предыдущие этапы завершились без
  ошибок [Translator.py](src/translator/translator.py)

### Правила генерации машинного кода:

В данном языке ассемблера, каждая инструкция программы представляется соответствующим машинным кодом. Ниже приведены
основные правила компиляции для преобразования инструкций в машинный код:

Одна инструкция - один машинный код: Каждая инструкция в программе ассемблера преобразуется в одну машинную инструкцию.
Это обеспечивает прямое соответствие между инструкциями и машинным кодом.

Прямое отображение: Для инструкций, которые однозначно соответствуют одной машинной инструкции, применяется прямое
отображение. Например, инструкция ldm DmemArg может быть преобразована в машинный код с кодом операции 2, если DmemArg -
указатель на память данных.

Обработка циклов: Для циклов с соблюдением парности используется особая логика компиляции. Возможно, потребуется
генерация дополнительных инструкций для правильной работы циклов в машинном коде. Например, в случае с циклом,
представленным в программе ассемблера `[...]`, требуется добавление инструкций для проверки условия цикла и перехода на
соответствующие адреса в программной памяти.

| Код инструкции | Программа | Машинный код |
|----------------|-----------|--------------|
| n              | [         | jz (k+1)     |
| ...            | ...       | ...          |
| k              | ]         | jmp n        |
| k+1            | ...       | ...          |

Никаких отображений регистров на память нет, так как нет самих регистров кроме аккумулятора.

## Система команд

Аргумент может быть

* NoneArg -- Не требуется
* DmemArg -- Указатель на память данных (dmem)
* PmemArg -- Pointer to program-memory (pmem)
* ValueArg -- Аргумент непосредственно
* IOPort -- Указатель на порт ввода вывода

| код | мнемоника | тип  аргумента | 	тактов |              	расшифровка	действия               | подробно                                          |
|:----|:---------:|:--------------:|:-------:|:------------------------------------------------:|---------------------------------------------------|
| 0   |   nop	    |    NoneArg	    |   1 	   |                операция простоя	                 | так как нет прерываний - определяет ошибку работы |
| 1   |   halt	   |    NoneArg	    |   0	    |                сигнал остановки	                 |                                                   |
| 2   |   ldm	    |    DmemArg	    |   2	    |      загрузить знач из памяти данных в acc	      | acc = dmem[arg]; c = const; z = (dmem[arg] == 0)  |
| 3   |   ldi	    |    ValueArg    |   	1    |      загрузить знач из памяти команд в acc	      | acc = arg; c = const; z = (arg == 0)              |
| 4   |   save	   |    DmemArg	    |   	2    | выгрузить значение аккумулятора в ячейку памяти	 | dmem[arg] = acc	                                  |
| 5   |   add	    |    DmemArg	    |   2	    |   добавить значение из ячейки к аккумулятору	    | acc + dmem[arg]                                   |
| 6   |   sub	    |    DmemArg	    |   2	    |     вычесть значение ячейки из аккумулятора	     | acc - dmem[arg]                                   |
| 7   |   cmp	    |    DmemArg	    |    2    |    проверка аккумулятора, выставить регистры     | 	z = (0 == acc); c = (acc < 0), acc > 0 ? 1 : 0   |
| 8   |   incr	   |    NoneArg	    |   1	    |      инкрементирует значение аккумулятора	       | x                                                 |
| 9   |   decr	   |    NoneArg	    |   1	    |      декрементирует значение аккумулятора	       | x                                                 |
| A   |   lsl	    |    ValueArg    |   1	    |  бит. сдвиг влево, устанавливается флаг c в с	   | x                                                 |
| B   |   lsr	    |    ValueArg    |   1	    |   бит. сдвиг вправо, устанавливается флаг c в	   | x                                                 |
| C   |   clrc	   |    NoneArg	    |   1	    |     сбрасывает значение флага переполнения	      | x                                                 |
| D   |   clrz	   |    NoneArg	    |   1	    |       сбрасывает значение нулевого флага		       | x                                                 |
| E   |   and	    |    DmemArg	    |   1	    |                логическое и acc	                 | acc & dmem[arg]                                   |
| F   |    or     |    DmemArg	    |   1	    |               логическое или    	                | acc  \| dmem[arg]                                 |
| 10  |   not	    |    NoneArg	    |   1	    |                логическое не acc	                | !acc                                              |
| 11  |   xor	    |    DmemArg	    |   1	    |               логический xor acc	                | acc xor dmem[arg]	                                |
| 12  |   jmp	    |    PmemArg	    |   1	    |            совершить переход на pmem	            | pc = pmem[arg]                                    |
| 13  |    jz	    |    PmemArg	    |   1	    |  переход на pmem при установленом zero флаге 1	  | z == 1 ? pc = pmem[arg]                           |
| 14  |   jnz	    |    PmemArg	    |    	    | переход на pmem при неустановленом zero флаге 0	 | z == 0 ? pc = pmem[arg]	                          |
| 15  |   mult	   |    DmemArg	    |   2	    |  умножить аккумулятор на знанч. из ячейки пам.	  | acc = acc * dmem[arg]                             |
| 16  |   dev	    |    DmemArg	    |   2	    | разделить аккумулятор на знанч. из ячейки пам.	  | acc = acc / dmem[arg]                             |
| 17  |   mod	    |    DmemArg	    |   2	    | остаток от деления аккумулятора на знанч.ячейки	 | acc = acc % dmem[arg]                             |
| 18  |   pow	    |    DmemArg	    |   2 	   |  возвести аккумулятор в степень из ячейки пам.	  | acc ^ dmem[arg]                                   |
| 19  |   sqrt	   |    NoneArg	    |   2	    |      подсчитать корень знач. аккумулятора	       | sqrt(acc)	                                        |
| 1A  |    in	    |    IOport	     |   2	    |  Ввод символьного значения с внешнего устр-ва	   | input >> dmem[arg]                                |
| 1B  |   out	    |    IOport	     |   2 	   |  Вывод символьного значения на внешнее устр-во	  | dmem[arg] >> output                               |
| 1C  |   ldref   |    DmemArg     |    3    |         Относительная загрузка из памяти         | dmem[arg] >> acc, acc = dmem[acc(16 младших бит)  | 

### Кодирование инструкций

* Машинные коды инструкций представлены в таблице.
* Машинное слово: 32 бита
    * Первые 16 бит (2 байта): Код команды
    * Оставшиеся 16 бит (2 байта): Аргумент

## Структура памяти

* Память инструкций
    * Размер: 65536 ячеек
    * Формат каждой ячейки: 32 бита (4 байта)
        * Первые 16 бит (2 байта): Код команды
        * Оставшиеся 16 бит (2 байта): Аргумент
* Память данных
    * Размер: 65536 ячеек
    * Формат каждой ячейки: 32 бита (4 байта)
        * Big-endian
        * Знаковое представление
        * Линейное адресное пространство
        * Реализуется списком чисел
* Аккумулятор
    * Размер: 32 бита (4 байта)
* Порты ввода-вывода
    * Пространство портов ввода-вывода состоит из 256 индивидуально адресуемых 16-битных ячеек (портов)
    * Адрес 1 байт
    * Размер каждого порта: 16 бит (2 байта)
    * Установлено 2 устройства
    * Порт ввода - Запись не дает никакого эффекта
    * Порт вывода - Чтение всегда возвращает 0
* Общая информация
    * Машинное слово: 32 бита (4 байта)

### Тонкости реализации

1. Транслятор подсчитывает, сколько памяти необходимо под хранение переменных.
   Неиспользованная часть адресного пространства - буферная.
2. В "буферной памяти" мы будем хранить промежуточные результаты вычислений
3. Для возможности подсчёта больших выражений мы перезаписываем одну ячейку буфера
   до тех пор, пока это возможно. В случае с выражением (1+2) + (3+4) Будут задействованы
   2 буферные ячейки.
4. После выполнения операции и записи переменной буфер считается освобождённым и используется
   с начала.
5. Иных вариантов оптимизации памяти и решения проблемы нехватки таковой я не вижу, т.к.
   нет ни кучи, ни стека, ни других регистров.**
6. Макс. длина строки 255 символов -> 255 регистров (+1 служебный регистр). Как в pascal. Именно столько выделит
   аллокатор
   под ячейку, которую необходимо ввести.

## Транслятор

stalin.py <input_file> <target_file>

