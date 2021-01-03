# Developing frameit-mmt

## Setting up a development environment

See [./DEVENV.md](./DEVENV.md).

## Design and Code-Related Comments

The `package-info.java` files and documentation within the Scala files should provide a good starting point for getting familiar with the source.

One of the main tasks of the frameit-mmt server is to interpret commands from the game engine and to create corresponding MMT content programmatically.
It's easy to do that wrong, please read <https://github.com/UniFormal/uniformal.github.io/wiki/Programmatically-creating-MMT-content-(checklist)>.