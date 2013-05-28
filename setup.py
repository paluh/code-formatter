try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

REQUIREMENTS = []
DEPENDENCY_LINKS = []
setup(
    name='code-formatter',
    author='Tomasz Rybarczyk',
    author_email='paluho@gmail.com',
    classifiers=[
        'Programming Language :: Python'
    ],
    description='',
    dependency_links=DEPENDENCY_LINKS,
    install_requires=REQUIREMENTS,
    url='https://bitbucket.org/paluh/code-formatter',
    packages=['code_formatter'],
    scripts=[],
    zip_safe=False,
    version = '2013.5.1',
)
