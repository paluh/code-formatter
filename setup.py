try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

setup(
    name='code-formatter',
    author='Tomasz Rybarczyk',
    author_email='paluho@gmail.com',
    classifiers=[
        'Programming Language :: Python'
    ],
    description='',
    dependency_links=[],
    install_requires=[],
    url='https://github.com/paluh/code-formatter',
    packages=['code_formatter'],
    scripts=[],
    zip_safe=False,
    version = '2013.8.1',
)
