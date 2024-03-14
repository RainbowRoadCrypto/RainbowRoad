# RainbowRoad

# WIP (Not Fully Tested Or Operational)
## Note: ** Modules DO compile ** (only a Few Weeks worth of work here)

RainbowRoad is a Haskell-based project that provides a comprehensive suite of tools and libraries for encoding, processing, and visualizing code using a unique color-based representation called "Rainbow Code". It leverages advanced techniques such as zero-knowledge proofs (ZKP) and PLONK for secure and efficient code processing.

## Features

- **Code Encoding**: RainbowRoad offers a novel encoding scheme that represents code elements using distinct colors. It supports encoding of characters, programming language constructs, smart contract elements, and op-codes.

- **Abstract Syntax Tree (AST) Processing**: The project includes functionality to parse code into an AST representation, enabling powerful code analysis and manipulation capabilities.

- **Intermediate Representation (IR)**: RainbowRoad defines an IR format that serves as a bridge between the AST and the color-based encoding. The IR allows for optimizations and transformations before the final encoding step.

- **PLONK Integration**: The project integrates with the PLONK (Permutations over Lagrange-bases for Oecumenical Noninteractive arguments of Knowledge) protocol for efficient zero-knowledge proof generation and verification.

- **Circuit Creation and Execution**: RainbowRoad provides a framework for creating and executing custom circuits based on the encoded code. It supports gate-level operations, conditional execution, and dynamic circuit generation.

- **Proof Generation and Verification**: The project includes functions for generating PLONK proofs based on the encoded code and verifying the correctness of the proofs using public keys.

- **Visualization**: RainbowRoad offers visualization capabilities to represent the encoded code in a visually appealing and intuitive manner, aiding in code comprehension and analysis.

## Installation

To install RainbowRoad, follow these steps:

1. Clone the repository:
   ```
   git clone https://github.com/your-username/RainbowRoad.git
   ```

2. Navigate to the project directory:
   ```
   cd RainbowRoad
   ```

3. Build the project using Cabal:
   ```
   cabal build
   ```

4. Run the tests to ensure everything is working correctly:
   ```
   cabal test
   ```

## Usage

RainbowRoad provides a command-line interface (CLI) for interacting with the various functionalities. Here are some examples:

- Encode a code file:
  ```
  rainbow encode --input code.js --output encoded.txt
  ```

- Decode an encoded file:
  ```
  rainbow decode --input encoded.txt --output decoded.js
  ```

- Generate a PLONK proof:
  ```
  rainbow prove --input encoded.txt --secret-key secret.key --output proof.data
  ```

- Verify a PLONK proof:
  ```
  rainbow verify --input encoded.txt --proof proof.data --public-key public.key
  ```

For more detailed usage instructions and additional commands, refer to the [User Guide](docs/user-guide.md).

## Contributing

Contributions to RainbowRoad are welcome! If you encounter any issues or have suggestions for improvements, please open an issue on the [GitHub repository](https://github.com/RainbowRoadCrypto/RainbowRoad/issues).

To contribute code changes, follow these steps:

1. Fork the repository.
2. Create a new branch for your feature or bug fix.
3. Make your changes and ensure that the tests pass.
4. Submit a pull request describing your changes.

Please adhere to the [Contribution Guidelines](CONTRIBUTING.md) when contributing to the project.

## License

RainbowRoad is released under the [MIT License](LICENSE).
