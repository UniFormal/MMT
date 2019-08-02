from setuptools import setup

setup(
    name='python_mmt',
    version='0.1',
    description='Python-MMT bridge',
    url='https://github.com/Uniformal/MMT',
    author='Florian Rabe',
    license='same as MMT license',
    classifiers=[
        'Intended Audience :: Science/Research',
        'Programming Language :: Python :: 3',
    ],
    keywords='MMT',
    packages=['python_mmt'],
    install_requires=['py4j'],
)
