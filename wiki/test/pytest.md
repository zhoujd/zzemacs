pytest
======

## Some URLs

    ## https://docs.pytest.org/en/7.1.x/getting-started.html

## Install pytest

    ## pytest requires: Python 3.7+ or PyPy3
    $ pip install -U pytest
    $ pytest --version
    pytest 7.1.2

## Create your first test

    $ cat > test_sample.py <<EOF
    # content of test_sample.py
    def func(x):
        return x + 1


    def test_answer():
        assert func(3) == 5

    EOF

    $ pytest

## Group multiple tests in a class

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

## pytest fixtures

    ## https://docs.pytest.org/en/7.1.x/reference/fixtures.html#fixtures
    $ pytest --fixtures   # shows builtin and custom fixtures
