import contextlib
import io
import logging
import os
import tempfile

import pytest
import src.emulator.emulator as machine
import src.translator.translator as translator


@pytest.mark.golden_test("golden/*.yml")
def test_translator_and_machine(golden, caplog):
    # Установим уровень отладочного вывода на DEBUG
    caplog.set_level(logging.DEBUG)
    # Создаём временную папку для тестирования приложения.
    with tempfile.TemporaryDirectory() as tmpdir:
        # Готовим имена файлов для входных и выходных данных.
        source = os.path.join(tmpdir, "code.ussr")
        input_stream = os.path.join(tmpdir, "input.txt")
        target = os.path.join(tmpdir, "binary")
        byte_description = os.path.join(tmpdir, "translator.log")
        # Записываем входные данные в файлы. Данные берутся из теста.
        with open(source, "w", encoding="utf-8") as file:
            file.write(golden["in_code"])
            file.close()
        with open(input_stream, "w", encoding="utf-8") as file:
            file.write(golden["in_stdin"])
            file.close()

        # Запускаем транслятор и собираем весь стандартный вывод в переменную
        # stdout
        with contextlib.redirect_stdout(io.StringIO()):
            translator.main(source, target, byte_description)
            print("============================================================")

        with contextlib.redirect_stdout(io.StringIO()) as stdout:
            machine.main(target, input_stream)

        with open(byte_description, encoding="utf-8") as file:
            code = file.read()
            file.close()

        # Проверяем, что ожидания соответствуют реальности.
        assert code == golden.out["out_code"]
        assert stdout.getvalue() == golden.out["out_stdout"]
        assert caplog.text == golden.out["out_log"]
