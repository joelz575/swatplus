pip3 install poetry
poetry install -v env use 3.8
poetry run pytest -s swatplus_test/test.py
