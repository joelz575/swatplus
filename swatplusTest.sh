curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3 -
export PATH=$HOME/.poetry/bin:$PATH
poetry install
poetry run pytest -s swatplus_test/test.py
