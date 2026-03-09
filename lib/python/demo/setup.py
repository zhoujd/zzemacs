from setuptools import setup, Extension

setup(
    name='demo',
    version='1.0',
    description='This is a demo package',
    ext_modules=[Extension('demo', sources=['demo.cpp'])]
)
