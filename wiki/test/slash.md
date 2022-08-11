Slash
=====

## Some URLs

    ## https://slash.readthedocs.io/en/master/
    ## https://github.com/intel/vaapi-fits
    ## https://buildmedia.readthedocs.org/media/pdf/slash/latest/slash.pdf

## Writing example Tests

    $ cat > test_addition.py <<EOF
    # test_addition.py

    import slash

    def test_addition():
        pass
    EOF

    $ slash run test_addition.py


## Using slash.parametrize

    ## https://slash.readthedocs.io/en/master/parameters.html
    class SomeTest(Test):
    @slash.parametrize('x', [1, 2, 3])
    def before(self, x):
    # ...
    @slash.parametrize('y', [4, 5, 6])
    def test(self, y):
    # ...
    @slash.parametrize('z', [7, 8, 9])
    def after(self, z):
    # ...

    ## Calls to super are handled automatically
    class BaseTest(Test):

    @slash.parametrize('base_parameter', [1, 2, 3])
    def before(self, base_parameter):
        # ....

    class DerivedTest(BaseTest):

        @slash.parametrize('derived_parameter', [4, 5, 6])
        def before(self, derived_parameter):
            super(DerivedTest, self).before() # note that base parameters aren't specified here
            # .....

## Customizing and Extending Slash

    ## https://slash.readthedocs.io/en/master/customizing_slash.html
    ## file: .slashrc
