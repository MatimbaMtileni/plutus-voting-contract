# Plutus Voting Smart Contract

A complete voting system implemented as a Plutus smart contract on Cardano.

## Features

- ✅ Secure voting with registered voters
- ✅ Multiple candidates support
- ✅ Admin-controlled voting process
- ✅ Vote counting and result calculation
- ✅ Prevent double voting
- ✅ Time-based voting periods

## Project Structure

src/
├── Voting/
│ ├── Types.hs -- Data types and definitions
│ ├── Validator.hs -- On-chain validator logic
│ ├── Helpers.hs -- Helper functions
│ └── OffChain.hs -- Off-chain code
└── Voting.hs -- Main module export


## Getting Started

### Prerequisites

- Nix package manager
- Cabal
- GHC 8.10.7 or later

### Development

1. **Enter development environment:**
   ```bash
   nix develop


Build the project:

cabal build
Run tests:

cabal test
Start REPL:

cabal repl
Usage Example

import Voting

-- Create voting parameters
params = VotingParams 
    { vpAdmin = "admin_pkh"
    , vpVoteToken = Nothing
    , vpMinAda = 2000000
    }

-- Initialize voting
initializeVoting params voters candidates "Election" 5000000

## Testing
Run the test suite:


cabal test voting-test

## License
Apache 2.0


## 14. `start-vscode.sh`

```bash
#!/bin/bash

echo "Setting up Plutus Voting Contract in VSCode..."

# Check if nix is installed
if ! command -v nix &> /dev/null; then
    echo "Error: Nix is not installed. Please install Nix first."
    echo "Visit: https://nixos.org/download.html"
    exit 1
fi

# Enter the development shell
echo "Entering development environment..."
nix develop

echo "Setup complete! You can now:"
echo "1. Open VSCode: code ."
echo "2. Build: cabal build"
echo "3. Test: cabal test"
echo "4. Run REPL: cabal repl"