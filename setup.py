try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup


CLASSIFIERS = [
    'Development Status :: 3 - Pre-Alpha',
    'Environment :: Console',
    'Intended Audience :: Developers',
    'License :: OSI Approved :: BSD License',
    'Operating System :: OS Independent',
    'Programming Language :: Python',
    'Programming Language :: Python :: 2.7',
    'Topic :: Software Development :: Code Generators',
    'Topic :: Software Development :: Libraries :: Python Modules']

setup(
    name='code-formatter',
    author='Tomasz Rybarczyk',
    author_email='paluho@gmail.com',
    classifiers=CLASSIFIERS,
    description='',
    dependency_links=[],
    install_requires=[],
    url='https://github.com/paluh/code-formatter',
    packages=['code_formatter'],
    scripts=[],
    test_suite='code_formatter.tests',
    zip_safe=False,
    version = '2013.8.1',
)
