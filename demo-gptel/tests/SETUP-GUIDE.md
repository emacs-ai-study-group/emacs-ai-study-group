# gptel Testing Setup Guide

This guide walks you through setting up and running the gptel test suite with both mock and integration testing.

## Quick Start

### Option 1: Unit Tests Only (No LLM Required)

```bash
cd demo-gptel/tests
emacs -Q -batch -L . -l gptel-ollama-ert.el -f ert-run-tests-batch-and-exit
```

This runs fast unit tests with mocks - **no Ollama or API keys needed!**

### Option 2: Full Integration Tests (Requires Ollama)

```bash
# 1. Install and start Ollama
ollama serve

# 2. Pull a model (in another terminal)
ollama pull llama3.2

# 3. Run tests
cd demo-gptel/tests
emacs -Q --eval "(progn
  (load-file \"gptel-ollama-ert.el\")
  (gptel-test-run-integration))"
```

## Detailed Setup

### Prerequisites

#### For Unit Tests (Mock-based)
- Emacs 27.1 or later
- gptel package from MELPA
- **That's it!** No external services needed

#### For Integration Tests
- All of the above, plus:
- Ollama installed from https://ollama.ai
- At least one model pulled
- Ollama server running

### Installing Ollama

#### macOS
```bash
brew install ollama
```

#### Linux
```bash
curl https://ollama.ai/install.sh | sh
```

#### Windows
Download from https://ollama.ai/download

### Pulling Models

```bash
# Recommended for testing (smaller, faster)
ollama pull llama3.2

# Alternative options
ollama pull codellama      # Better for code generation
ollama pull mistral        # General purpose
ollama pull deepseek-coder # Specialized for coding
```

### Starting Ollama Server

```bash
# Start in foreground
ollama serve

# Or start as background service (Linux/macOS)
ollama serve &

# Check if running
curl http://localhost:11434/api/tags
```

Should return JSON with available models.

## Running Tests

### Interactive Testing (Emacs GUI)

1. **Open Emacs and load test file:**
   ```elisp
   M-x find-file RET demo-gptel/tests/gptel-ollama-ert.el RET
   M-x eval-buffer RET
   ```

2. **Run specific test sets:**
   ```elisp
   ;; All tests
   M-x ert RET t RET

   ;; Unit tests only (fast)
   M-x ert RET (tag :unit) RET

   ;; Mock tests only
   M-x ert RET (tag :mock) RET

   ;; Integration tests (requires Ollama)
   M-x ert RET (tag :integration) RET
   ```

3. **Or use convenience functions:**
   ```elisp
   M-x gptel-test-run-all           ; All tests
   M-x gptel-test-run-unit          ; Unit only
   M-x gptel-test-run-integration   ; Integration only
   M-x gptel-test-report            ; View test environment
   ```

### Batch Testing (Command Line)

```bash
cd demo-gptel/tests

# Run all tests
emacs -Q -batch -L . -l gptel-ollama-ert.el \
  -f ert-run-tests-batch-and-exit

# Run only unit tests
emacs -Q -batch -L . -l gptel-ollama-ert.el \
  --eval "(ert-run-tests-batch-and-exit '(tag :unit))"

# Run only mock tests
emacs -Q -batch -L . -l gptel-ollama-ert.el \
  --eval "(ert-run-tests-batch-and-exit '(tag :mock))"
```

### Interactive Org-Mode Testing

```bash
cd demo-gptel/docs
emacs testing-guide.org
```

Then execute code blocks with `C-c C-c` to step through examples interactively.

## Test Categories

### `:unit` - Unit Tests
- **Fast** (< 1 second)
- **No external dependencies**
- Tests internal logic, configuration, helpers

**Examples:**
- Package loading
- Backend creation
- Directory creation
- Code extraction/validation

### `:mock` - Mock Tests
- **Fast** (< 1 second)
- **No external dependencies**
- Uses mock framework to simulate LLM responses
- Tests interaction patterns without real API calls

**Examples:**
- Mock responses and callbacks
- Spy on API calls
- Test invalid code handling
- Test error conditions

### `:integration` - Integration Tests
- **Slow** (5-60 seconds per test)
- **Requires Ollama server**
- Makes real API calls to Ollama
- Non-deterministic results

**Examples:**
- Simple questions
- Code generation
- Math reasoning
- Response validation

### `:slow` - Slow Tests
- May take 30+ seconds
- Usually also tagged with `:integration`

## Understanding Test Results

### Successful Test Output

```
Running 1 tests (2025-11-06 ...)

   passed  1/1  gptel-test-mock-simple-response

Ran 1 tests, 1 results as expected (2025-11-06 ...)
```

### Failed Test Output

```
Running 1 tests (2025-11-06 ...)

F gptel-test-mock-simple-response
    (ert-test-failed
     ((should (equal response "Hi there!"))
      :form (equal nil "Hi there!")
      :value nil))
```

Indicates what assertion failed and the actual values.

### Skipped Test Output

```
s gptel-test-ollama-server-running
  Test skipped: Ollama not available
```

Tests tagged with `:integration` will skip if Ollama isn't running.

## Working with Mocks

### Enable Mock Framework

```elisp
(require 'gptel-ollama-ert)

;; Enable mocking
(gptel-test--enable-mock)

;; Add mock responses
(gptel-test--add-mock "What is 2+2?" "The answer is 4.")

;; Now gptel-request will use mocks
(gptel-request "What is 2+2?"
  :callback (lambda (response info)
             (message "Got: %s" response)))
;; => "Got: The answer is 4."

;; Disable when done
(gptel-test--disable-mock)
(gptel-test--clear-mocks)
```

### Inspect Spy Data

```elisp
;; Enable spy
(gptel-test--enable-mock)
(gptel-test--clear-mocks)

;; Make some requests
(gptel-request "Prompt 1" :callback (lambda (r i) nil))
(gptel-request "Prompt 2" :callback (lambda (r i) nil))

;; Inspect what was called
gptel-test--spy-calls
;; => ((:prompt "Prompt 2" :args (...) :time (...))
;;     (:prompt "Prompt 1" :args (...) :time (...)))
```

## Writing Your Own Tests

### Example: Simple Unit Test

```elisp
(ert-deftest my-simple-test ()
  "Test description."
  :tags '(:unit)
  (should (= (+ 1 1) 2)))
```

### Example: Mock-Based Test

```elisp
(ert-deftest my-mock-test ()
  "Test with mock LLM."
  :tags '(:unit :mock)
  (unwind-protect
      (progn
        (gptel-test--clear-mocks)
        (gptel-test--enable-mock)
        (gptel-test--add-mock "Test prompt" "Test response")

        (let ((result nil))
          (gptel-request "Test prompt"
            :callback (lambda (resp info)
                       (setq result resp)))

          (should (equal result "Test response"))))

    ;; Cleanup
    (gptel-test--disable-mock)
    (gptel-test--clear-mocks)))
```

### Example: Integration Test

```elisp
(ert-deftest my-integration-test ()
  "Test with real Ollama."
  :tags '(:integration :slow)
  (skip-unless (gptel-test--check-ollama-available))

  (gptel-test--setup-ollama-backend)

  (let* ((result (gptel-test--sync-request "Say hello"))
         (response (car result))
         (success (cdr result)))

    (should success)
    (should (stringp response))
    (should (> (length response) 0))))
```

## Troubleshooting

### "gptel not loaded"

```elisp
;; Install gptel
M-x package-refresh-contents RET
M-x package-install RET gptel RET

;; Then reload test file
M-x eval-buffer
```

### "Ollama not available"

```bash
# Check if Ollama is running
ps aux | grep ollama

# Start if not running
ollama serve &

# Test connection
curl http://localhost:11434/api/tags
```

### "Test timed out"

Integration tests wait up to 60 seconds for responses. If timeouts occur:

1. **Check Ollama is responding:**
   ```bash
   curl http://localhost:11434/api/tags
   ```

2. **Check model is pulled:**
   ```bash
   ollama list
   ```

3. **Increase timeout:**
   ```elisp
   (setq gptel-test-timeout 120)  ; 2 minutes
   ```

### "Mock tests failing"

Make sure to clear mocks between tests:

```elisp
;; At start of each mock test
(gptel-test--clear-mocks)
(gptel-test--enable-mock)

;; At end (in unwind-protect)
(gptel-test--disable-mock)
(gptel-test--clear-mocks)
```

### "Integration tests always skipping"

Check the skip condition:

```elisp
;; This should return non-nil
(gptel-test--check-ollama-available)

;; If nil, check:
(executable-find "ollama")  ; Should find Ollama binary
```

## Test Results

Test results are saved to `demo-gptel/tests/results/` directory with timestamps.

Example result file: `simple-request-20251106-153022.txt`

```
Prompt: What is 2+2?

Response: The answer is 4.

Success: t
```

## CI/Automation

### GitHub Actions Example

```yaml
name: Test gptel

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Install Emacs
        run: sudo apt-get install -y emacs

      - name: Run unit tests
        run: |
          cd demo-gptel/tests
          emacs -Q -batch -L . -l gptel-ollama-ert.el \
            --eval "(ert-run-tests-batch-and-exit '(tag :unit))"
```

For integration tests in CI, you'd also need to install and start Ollama.

## Best Practices

1. **Run unit tests frequently** - They're fast and catch basic issues
2. **Run integration tests before commits** - Validate real behavior
3. **Use mocks for rapid iteration** - Faster feedback during development
4. **Tag tests appropriately** - Makes it easy to run specific suites
5. **Always cleanup in unwind-protect** - Prevents test pollution
6. **Save integration results** - Helpful for analyzing AI behavior over time

## Additional Resources

- **Testing Guide (Org-mode):** `demo-gptel/docs/testing-guide.org`
- **Interactive Demo:** `demo-gptel/docs/demo.org`
- **ERT Documentation:** `C-h i m ert RET` (in Emacs)
- **gptel Documentation:** https://github.com/karthink/gptel
- **Elisp Best Practices:** `docs/elisp-development.md`

## Getting Help

If you encounter issues:

1. Check this guide
2. Run `M-x gptel-test-report` to see environment info
3. Look at test result files in `results/` directory
4. Check the main project issues
5. Ask in the Emacs AI Study Group

---

**Last Updated:** 2025-11-06
**Version:** 1.0
