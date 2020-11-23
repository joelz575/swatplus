pip3 install poetry
poetry install -v env use 3.9
poetry run pytest -s swatplus_test/test.py
