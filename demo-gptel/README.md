# gptel Demo

This project demonstrates [gptel](https://github.com/karthink/gptel), a simple LLM client for Emacs, with comprehensive testing examples using both mocks and local LLM integration.

## Overview

gptel is a simple, no-frills LLM client for Emacs that supports:
- ChatGPT (OpenAI API)
- Claude (Anthropic API)
- Gemini (Google AI API)
- Local models via Ollama, Llama.cpp, and others

## What's Included

### 📚 Documentation

- **[docs/demo.org](./docs/demo.org)** - Interactive demonstration with executable babel blocks
- **[docs/testing-guide.org](./docs/testing-guide.org)** - Comprehensive testing guide with validation examples
- **[elisp/setup.el](./elisp/setup.el)** - Complete setup configuration for all backends

### 🧪 Testing Suite

- **[tests/gptel-ollama-ert.el](./tests/gptel-ollama-ert.el)** - **NEW!** Comprehensive ERT test suite with:
  - 16 ERT tests covering unit, mock, and integration testing
  - Mock/spy framework for testing without LLM
  - Integration tests for Ollama
  - Proper test tagging (`:unit`, `:mock`, `:integration`, `:slow`)

- **[tests/SETUP-GUIDE.md](./tests/SETUP-GUIDE.md)** - **NEW!** Complete setup and usage guide
- **[tests/README.md](./tests/README.md)** - Overview of all test files
- **[tests/test-automated-ollama.el](./tests/test-automated-ollama.el)** - Automated AI code generation testing
- **[tests/results/](./tests/results/)** - Captured test results with timestamps

## Quick Start

### Option 1: Try It Interactively (Org-mode)

```bash
cd docs
emacs testing-guide.org
# Execute code blocks with C-c C-c
```

### Option 2: Run Unit Tests (No LLM Required)

```bash
cd tests
emacs -Q -batch -L . -l gptel-ollama-ert.el \
  -f ert-run-tests-batch-and-exit
```

This runs **fast unit tests with mocks** - no Ollama or API keys needed!

### Option 3: Run Integration Tests (Requires Ollama)

```bash
# 1. Start Ollama
ollama serve

# 2. Pull a model
ollama pull llama3.2

# 3. Run tests
cd tests
emacs -Q --eval "(progn
  (load-file \"gptel-ollama-ert.el\")
  (gptel-test-run-integration))"
```

## Features

### 🎯 Mock Testing Framework

Test your AI-integrated code without making real API calls:

```elisp
;; Enable mocking
(gptel-test--enable-mock)

;; Add mock responses
(gptel-test--add-mock "What is 2+2?" "The answer is 4.")

;; Test your code with predictable responses
(gptel-request "What is 2+2?"
  :callback (lambda (response info)
             (message "Got: %s" response)))

;; Spy on calls
gptel-test--spy-calls  ; See all API calls made
```

### 🔬 Integration Testing

Test with real Ollama LLM:

```elisp
(ert-deftest my-integration-test ()
  :tags '(:integration :slow)
  (skip-unless (gptel-test--check-ollama-available))

  (let* ((result (gptel-test--sync-request "Say hello"))
         (response (car result))
         (success (cdr result)))
    (should success)
    (should (stringp response))))
```

### ✅ Code Validation

Automatically validate AI-generated Elisp code:

```elisp
(defun validate-elisp-function (code)
  "Validate CODE for common AI mistakes."
  ;; Checks for Python syntax (%, ==, return)
  ;; Validates parentheses
  ;; Tests evaluation
  ...)
```

## Test Categories

- **:unit** - Fast tests, no external dependencies
- **:mock** - Tests using mock LLM responses
- **:integration** - Tests with real Ollama (slow, requires server)
- **:slow** - Tests that may take 30+ seconds

## Setup

### Basic Setup (Any LLM Provider)

1. Install gptel from MELPA:
   ```elisp
   M-x package-install RET gptel RET
   ```

2. Load configuration:
   ```elisp
   (load-file "demo-gptel/elisp/setup.el")
   ```

3. Configure your preferred backend (see [elisp/setup.el](./elisp/setup.el))

### Local LLM Setup (Ollama)

See [tests/SETUP-GUIDE.md](./tests/SETUP-GUIDE.md) for detailed instructions.

**Quick version:**

```bash
# Install Ollama
brew install ollama  # macOS
# or visit https://ollama.ai for other platforms

# Start server
ollama serve

# Pull a model
ollama pull llama3.2
```

## Documentation

- **[Elisp Development Guide](../docs/elisp-development.md)** - Best practices for developing Elisp with AI coding agents
- **[Testing Guide (Org)](./docs/testing-guide.org)** - Interactive testing examples
- **[Setup Guide](./tests/SETUP-GUIDE.md)** - Detailed testing setup
- **[Test Results README](./tests/README.md)** - Understanding test outputs

## Resources

- [gptel GitHub](https://github.com/karthink/gptel)
- [gptel Manual](https://github.com/karthink/gptel/wiki)
- [Ollama](https://ollama.ai)
- [ERT Manual](https://www.gnu.org/software/emacs/manual/html_node/ert/)

## Project Structure

```
demo-gptel/
├── README.md                      # This file
├── docs/
│   ├── demo.org                   # Interactive demo
│   └── testing-guide.org          # Testing guide with examples
├── elisp/
│   └── setup.el                   # Configuration examples
└── tests/
    ├── SETUP-GUIDE.md             # Testing setup guide
    ├── README.md                  # Test overview
    ├── gptel-ollama-ert.el        # ERT test suite (NEW!)
    ├── test-automated-ollama.el   # Automated testing
    ├── test-ollama-fizzbuzz.el    # Static unit tests
    └── results/                   # Test result files
```

## Contributing

We welcome contributions! Areas of interest:

- Additional test cases
- Examples with different LLM providers
- Documentation improvements
- Common AI mistake patterns
- Performance benchmarks

## Examples from Test Results

Real examples of AI-generated code captured by our tests:

### Example 1: Common Mistake (Python Syntax)

**AI Generated:**
```elisp
(defun fizzbuzz (n)
  (if (i % 3 == 0) "Fizz" "Buzz"))
```

**Issues:**
- Uses `%` operator (Python/C syntax, not Elisp)
- Uses `==` operator (not Elisp)

**Corrected:**
```elisp
(defun fizzbuzz (n)
  (if (zerop (mod i 3)) "Fizz" "Buzz"))
```

### Example 2: Successful Generation

**AI Generated:**
```elisp
(defun add-two-numbers (a b)
  "Add A and B."
  (+ a b))
```

**Status:** ✓ Valid Elisp, evaluates successfully

See [tests/results/](./tests/results/) for more real examples!

## Next Steps

1. **Try the interactive guide:** Open `docs/testing-guide.org` and execute the babel blocks
2. **Run unit tests:** See what mocking looks like without needing an LLM
3. **Install Ollama:** Try integration tests with a real local LLM
4. **Write your own tests:** Use the examples as templates
5. **Share your findings:** Contribute back to the study group

---

**Part of the [Emacs AI Study Group](../../README.md)**
