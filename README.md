# Experiments with Functional C++

Just messing around with the exercises from the book "Functional Programming in C++" by Ivan Čukić.
https://www.manning.com/books/functional-programming-in-c-plus-plus

# Building

The project uses submodules for some third-party dependencies, such as Catch2.
To update the submodules:
```bash
git submodule update --init
```

### Using pre-commit hooks
This project uses the [pre-commit](https://pre-commit.com/) tool to lint git
commits before they are made. Install these as follows:

```bash
# Install the pre-commit tool
# It's also available through tools such as homebrew.
pip install pre-commit

# Install the pre-commit hooks to the project
cd <path-to-source-folder>
pre-commit install --install-hooks

# Install git commit linter to the project
pre-commit install --hook-type commit-msg
```

You can see a list of the checks that are performed by inspecting the
`.pre-commit-config.yaml` file.
