pytest
======

## Some URLs

```
## https://docs.pytest.org/en/7.1.x/getting-started.html
```

## Install pytest

```
## pytest requires: Python 3.7+ or PyPy3
$ pip install -U pytest
$ pytest --version
pytest 7.1.2
```

## Create your first test

```
$ cat > test_sample.py <<EOF
# content of test_sample.py
def func(x):
    return x + 1


def test_answer():
    assert func(3) == 5

EOF

$ pytest
```

## Group multiple tests in a class

```
$ cat > test_class.py <<EOF
# content of test_class.py
class TestClass:
    def test_one(self):
        x = "this"
        assert "h" in x

    def test_two(self):
        x = "hello"
        assert hasattr(x, "check")
EOF

$ pytest -q test_class.py
$ pytest -k TestClass -q
```

## pytest fixtures

```
## https://docs.pytest.org/en/7.1.x/reference/fixtures.html#fixtures
$ pytest --fixtures   # shows builtin and custom fixtures
```

## displaying timestamps alongside each test

```
$ pip install pytest-timestamps
```

## disable a test using pytest

```
@pytest.mark.skip(reason="no way of currently testing this")
def test_the_unknown():
    ...

import sys
@pytest.mark.skipif(sys.version_info < (3,3), reason="requires python3.3")
def test_function():
    ...

def test_valid_counting_number():
    number = random.randint(1,5)
    if number == 5:
        pytest.skip('Five is right out')
    assert number <= 3
```

## custom pytest markers

```
## https://docs.pytest.org/en/6.2.x/example/markers.html
@pytest.mark.my_unit_test
def test_that_unit():
...

@pytest.mark.my_functional_test
def test_that_function():
...

And then, to run only one set of unit tests, as example: pytest -m my_unit_test
Inverse, if you want to run all tests, except one set: pytest -m "not my_unit_test"
```
