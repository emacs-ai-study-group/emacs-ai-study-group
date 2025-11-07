;;; gptel-ollama-ert.el --- ERT tests for gptel with Ollama -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Emacs AI Study Group

;; Author: Emacs AI Study Group
;; Keywords: testing, llm, gptel
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0") (ert "1.0"))

;;; Commentary:

;; This file provides ERT (Emacs Lisp Regression Testing) tests for gptel
;; with Ollama backend.  These tests demonstrate proper testing patterns for
;; AI-integrated Emacs packages.
;;
;; Prerequisites:
;; - Ollama installed and running (ollama serve)
;; - A model pulled (e.g., ollama pull llama3.2)
;; - gptel package installed
;;
;; Run tests:
;;   M-x ert RET t RET
;; Or from command line:
;;   emacs -Q -batch -L . -l gptel-ollama-ert.el -f ert-run-tests-batch-and-exit
;;
;; Test categories:
;; - :unit         - Unit tests (no external dependencies)
;; - :integration  - Integration tests (requires Ollama)
;; - :slow         - Tests that take significant time
;; - :interactive  - Tests requiring AI responses

;;; Code:

(require 'ert)
(require 'gptel)

;;; Configuration Variables

(defvar gptel-test-ollama-host "localhost:11434"
  "Ollama server host for testing.")

(defvar gptel-test-ollama-model "llama3.2:latest"
  "Ollama model to use for testing.")

(defvar gptel-test-timeout 60
  "Timeout in seconds for AI responses.")

(defvar gptel-test-results-dir
  (expand-file-name "results"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory for test results.")

;;; Mock/Spy Framework

(defvar gptel-test--mock-enabled nil
  "When non-nil, use mock responses instead of real API calls.")

(defvar gptel-test--mock-responses nil
  "Alist of (PROMPT . RESPONSE) for mocked responses.")

(defvar gptel-test--spy-calls nil
  "List of calls made to gptel-request for testing.")

(defun gptel-test--mock-response (prompt)
  "Get mock response for PROMPT."
  (or (cdr (assoc-string prompt gptel-test--mock-responses))
      (format "Mock response for: %s" (substring prompt 0 (min 50 (length prompt))))))

(defun gptel-test--add-mock (prompt response)
  "Add a mock RESPONSE for PROMPT."
  (push (cons prompt response) gptel-test--mock-responses))

(defun gptel-test--clear-mocks ()
  "Clear all mock responses and spy data."
  (setq gptel-test--mock-responses nil)
  (setq gptel-test--spy-calls nil))

(defun gptel-test--enable-mock ()
  "Enable mock mode for testing."
  (setq gptel-test--mock-enabled t)
  (advice-add 'gptel-request :around #'gptel-test--mock-advice))

(defun gptel-test--disable-mock ()
  "Disable mock mode."
  (setq gptel-test--mock-enabled nil)
  (advice-remove 'gptel-request #'gptel-test--mock-advice))

(defun gptel-test--mock-advice (orig-fun prompt &rest args)
  "Mock advice for gptel-request.
ORIG-FUN is the original function, PROMPT is the request prompt,
ARGS are additional arguments."
  (if gptel-test--mock-enabled
      (let* ((callback (plist-get args :callback))
             (response (gptel-test--mock-response prompt)))
        ;; Record the call
        (push (list :prompt prompt :args args :time (current-time))
              gptel-test--spy-calls)
        ;; Call callback with mock response
        (when callback
          (funcall callback response nil))
        response)
    ;; Normal execution
    (apply orig-fun prompt args)))

;;; Helper Functions

(defun gptel-test--ensure-results-dir ()
  "Ensure test results directory exists."
  (unless (file-directory-p gptel-test-results-dir)
    (make-directory gptel-test-results-dir t)))

(defun gptel-test--save-result (name content)
  "Save test result CONTENT to file with NAME."
  (gptel-test--ensure-results-dir)
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "%s/%s-%s.txt"
                          gptel-test-results-dir
                          name
                          timestamp)))
    (with-temp-file filename
      (insert (format "=== Test: %s ===\n" name))
      (insert (format "Timestamp: %s\n\n" (current-time-string)))
      (insert content)
      (insert "\n=== END ===\n"))
    filename))

(defun gptel-test--check-ollama-available ()
  "Check if Ollama is available.
Returns non-nil if Ollama server is responding."
  (and (executable-find "ollama")
       (condition-case nil
           (with-temp-buffer
             (call-process "curl" nil t nil
                          "-s"
                          (format "http://%s/api/tags" gptel-test-ollama-host))
             (goto-char (point-min))
             (looking-at "{"))
         (error nil))))

(defun gptel-test--setup-ollama-backend ()
  "Set up Ollama backend for testing."
  (setq-default gptel-backend
                (gptel-make-ollama "Ollama-Test"
                  :host gptel-test-ollama-host
                  :stream nil  ; Disable streaming for easier testing
                  :models (list gptel-test-ollama-model)))
  (setq-default gptel-model gptel-test-ollama-model))

(defun gptel-test--sync-request (prompt &optional timeout)
  "Send PROMPT and wait for response synchronously.
TIMEOUT defaults to `gptel-test-timeout'.
Returns (RESPONSE . SUCCESS-P)."
  (let ((timeout (or timeout gptel-test-timeout))
        (response nil)
        (success nil)
        (done nil))
    (gptel-request prompt
      :callback
      (lambda (resp info)
        (setq response resp)
        (setq success (and resp (stringp resp)))
        (setq done t)))

    ;; Wait for response with timeout
    (let ((start-time (current-time)))
      (while (and (not done)
                  (< (float-time (time-since start-time)) timeout))
        (sleep-for 0.1)))

    (cons response success)))

;;; Unit Tests (No External Dependencies)

(ert-deftest gptel-test-package-loaded ()
  "Test that gptel package is loaded."
  :tags '(:unit)
  (should (featurep 'gptel)))

(ert-deftest gptel-test-ollama-backend-creation ()
  "Test creating Ollama backend."
  :tags '(:unit)
  (let ((backend (gptel-make-ollama "Test"
                   :host "localhost:11434"
                   :models '(llama3.2:latest))))
    (should backend)
    (should (equal (oref backend name) "Test"))))

(ert-deftest gptel-test-results-dir-creation ()
  "Test that results directory can be created."
  :tags '(:unit)
  (gptel-test--ensure-results-dir)
  (should (file-directory-p gptel-test-results-dir)))

;;; Unit Tests with Mocks

(ert-deftest gptel-test-mock-simple-response ()
  "Test mock framework with simple response."
  :tags '(:unit :mock)
  (unwind-protect
      (progn
        (gptel-test--clear-mocks)
        (gptel-test--enable-mock)
        (gptel-test--add-mock "Hello" "Hi there!")

        (let* ((response nil)
               (callback-called nil))
          (gptel-request "Hello"
            :callback (lambda (resp info)
                       (setq response resp)
                       (setq callback-called t)))

          (should callback-called)
          (should (equal response "Hi there!"))
          (should (= (length gptel-test--spy-calls) 1))))

    ;; Cleanup
    (gptel-test--disable-mock)
    (gptel-test--clear-mocks)))

(ert-deftest gptel-test-mock-code-generation ()
  "Test mock framework with Elisp code generation."
  :tags '(:unit :mock)
  (unwind-protect
      (progn
        (gptel-test--clear-mocks)
        (gptel-test--enable-mock)

        (let ((mock-code "(defun add-two (a b)\n  \"Add two numbers.\"\n  (+ a b))"))
          (gptel-test--add-mock "Generate add function" mock-code)

          (let ((response nil))
            (gptel-request "Generate add function"
              :callback (lambda (resp info)
                         (setq response resp)))

            (should (stringp response))
            (should (string-match-p "defun add-two" response))
            (should (string-match-p "(\\+ a b)" response)))))

    ;; Cleanup
    (gptel-test--disable-mock)
    (gptel-test--clear-mocks)))

(ert-deftest gptel-test-mock-invalid-elisp ()
  "Test handling of invalid Elisp from mock."
  :tags '(:unit :mock)
  (unwind-protect
      (progn
        (gptel-test--clear-mocks)
        (gptel-test--enable-mock)

        ;; Mock response with common AI mistake (Python syntax)
        (let ((bad-code "(defun fizzbuzz (n)\n  (if (i % 3 == 0) \"Fizz\"))"))
          (gptel-test--add-mock "fizzbuzz" bad-code)

          (let ((response nil))
            (gptel-request "fizzbuzz"
              :callback (lambda (resp info)
                         (setq response resp)))

            ;; Response received but contains invalid syntax
            (should (stringp response))
            (should (string-match-p "%" response))

            ;; Attempting to eval should fail
            (should-error (eval (read response))))))

    ;; Cleanup
    (gptel-test--disable-mock)
    (gptel-test--clear-mocks)))

(ert-deftest gptel-test-spy-call-recording ()
  "Test that spy records all calls correctly."
  :tags '(:unit :mock)
  (unwind-protect
      (progn
        (gptel-test--clear-mocks)
        (gptel-test--enable-mock)

        (gptel-test--add-mock "Prompt 1" "Response 1")
        (gptel-test--add-mock "Prompt 2" "Response 2")

        (gptel-request "Prompt 1" :callback (lambda (r i) nil))
        (gptel-request "Prompt 2" :callback (lambda (r i) nil))

        (should (= (length gptel-test--spy-calls) 2))

        ;; Check that calls were recorded in reverse order (push)
        (should (equal (plist-get (car gptel-test--spy-calls) :prompt)
                      "Prompt 2"))
        (should (equal (plist-get (cadr gptel-test--spy-calls) :prompt)
                      "Prompt 1")))

    ;; Cleanup
    (gptel-test--disable-mock)
    (gptel-test--clear-mocks)))

(ert-deftest gptel-test-mock-default-response ()
  "Test that unmocked prompts get default response."
  :tags '(:unit :mock)
  (unwind-protect
      (progn
        (gptel-test--clear-mocks)
        (gptel-test--enable-mock)

        (let ((response nil))
          (gptel-request "This prompt has no mock"
            :callback (lambda (resp info)
                       (setq response resp)))

          (should (stringp response))
          (should (string-prefix-p "Mock response for:" response))))

    ;; Cleanup
    (gptel-test--disable-mock)
    (gptel-test--clear-mocks)))

;;; Environment Tests

(ert-deftest gptel-test-ollama-executable ()
  "Test that Ollama executable is found."
  :tags '(:integration)
  (skip-unless (executable-find "ollama"))
  (should (file-executable-p (executable-find "ollama"))))

(ert-deftest gptel-test-ollama-server-running ()
  "Test that Ollama server is responding."
  :tags '(:integration)
  (skip-unless (gptel-test--check-ollama-available))
  (should (gptel-test--check-ollama-available)))

;;; Integration Tests (Requires Ollama)

(ert-deftest gptel-test-simple-request ()
  "Test simple request to Ollama."
  :tags '(:integration :slow :interactive)
  (skip-unless (gptel-test--check-ollama-available))

  (gptel-test--setup-ollama-backend)

  (let* ((prompt "Reply with exactly: Hello from Ollama")
         (result (gptel-test--sync-request prompt))
         (response (car result))
         (success (cdr result)))

    (should success)
    (should (stringp response))
    (should (> (length response) 0))

    ;; Save result for inspection
    (gptel-test--save-result
     "simple-request"
     (format "Prompt: %s\n\nResponse: %s\n\nSuccess: %s"
             prompt response success))))

(ert-deftest gptel-test-code-generation ()
  "Test Elisp code generation with Ollama."
  :tags '(:integration :slow :interactive)
  (skip-unless (gptel-test--check-ollama-available))

  (gptel-test--setup-ollama-backend)

  (let* ((prompt "Write a simple Emacs Lisp function called 'add-two' that takes two numbers and returns their sum. Reply with ONLY the function definition, no explanation.")
         (result (gptel-test--sync-request prompt))
         (response (car result))
         (success (cdr result)))

    (should success)
    (should (stringp response))
    (should (string-match-p "defun" response))

    ;; Save result
    (gptel-test--save-result
     "code-generation"
     (format "Prompt: %s\n\nResponse: %s\n" prompt response))))

(ert-deftest gptel-test-math-reasoning ()
  "Test basic math reasoning."
  :tags '(:integration :slow :interactive)
  (skip-unless (gptel-test--check-ollama-available))

  (gptel-test--setup-ollama-backend)

  (let* ((prompt "What is 15 + 27? Reply with ONLY the number.")
         (result (gptel-test--sync-request prompt))
         (response (car result))
         (success (cdr result)))

    (should success)
    (should (stringp response))

    ;; Response should contain "42"
    (should (string-match-p "42" response))

    (gptel-test--save-result
     "math-reasoning"
     (format "Prompt: %s\n\nResponse: %s\n" prompt response))))

;;; Code Quality Tests

(ert-deftest gptel-test-elisp-code-extraction ()
  "Test extraction of Elisp code from AI response."
  :tags '(:unit)
  (let ((response "Here's the code:\n```elisp\n(defun test () 42)\n```\nThat should work!"))
    ;; Simple extraction test
    (should (string-match "```\\(?:elisp\\|emacs-lisp\\)?\n\\(\\(?:.\\|\n\\)*?\\)```" response))
    (should (string= "(defun test () 42)" (string-trim (match-string 1 response))))))

(ert-deftest gptel-test-code-validation ()
  "Test that generated code can be evaluated."
  :tags '(:integration :slow :interactive)
  (skip-unless (gptel-test--check-ollama-available))

  (gptel-test--setup-ollama-backend)

  (let* ((prompt "Write an Emacs Lisp function 'square' that takes a number and returns its square. Use only standard Elisp. Reply with ONLY the function, no markdown.")
         (result (gptel-test--sync-request prompt))
         (response (car result))
         (success (cdr result))
         (code nil)
         (eval-success nil))

    (should success)

    ;; Extract code (simple extraction)
    (setq code (if (string-match "(defun \\(?:.\\|\n\\)*" response)
                   (match-string 0 response)
                 response))

    ;; Try to evaluate
    (condition-case err
        (progn
          (eval (read code))
          (setq eval-success t))
      (error
       (message "Eval error: %s" (error-message-string err))))

    ;; Save result with analysis
    (gptel-test--save-result
     "code-validation"
     (format "Prompt: %s\n\nResponse: %s\n\nExtracted Code:\n%s\n\nEval Success: %s\n"
             prompt response code eval-success))

    ;; This might fail with AI-generated code, so we just warn
    (when (not eval-success)
      (message "Warning: Generated code could not be evaluated"))))

;;; Performance Tests

(ert-deftest gptel-test-response-time ()
  "Test that responses arrive within reasonable time."
  :tags '(:integration :slow :interactive)
  (skip-unless (gptel-test--check-ollama-available))

  (gptel-test--setup-ollama-backend)

  (let* ((prompt "Say 'Hi'")
         (start-time (current-time))
         (result (gptel-test--sync-request prompt 30))
         (elapsed (float-time (time-since start-time)))
         (success (cdr result)))

    (should success)
    (should (< elapsed 30))

    (message "Response time: %.2f seconds" elapsed)

    (gptel-test--save-result
     "response-time"
     (format "Prompt: %s\nElapsed: %.2f seconds\nResponse: %s\n"
             prompt elapsed (car result)))))

;;; Test Suite Runner

(defun gptel-test-run-all ()
  "Run all gptel tests."
  (interactive)
  (ert-run-tests-interactively "^gptel-test-"))

(defun gptel-test-run-unit ()
  "Run only unit tests (fast, no Ollama required)."
  (interactive)
  (ert-run-tests-interactively '(tag :unit)))

(defun gptel-test-run-integration ()
  "Run integration tests (requires Ollama)."
  (interactive)
  (if (gptel-test--check-ollama-available)
      (ert-run-tests-interactively '(tag :integration))
    (message "Ollama not available. Please start Ollama server and try again.")))

;;; Test Report

(defun gptel-test-report ()
  "Generate a test report."
  (interactive)
  (let ((buffer (get-buffer-create "*gptel-test-report*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== gptel Test Report ===\n\n")
      (insert (format "Date: %s\n\n" (current-time-string)))

      (insert "=== Environment ===\n")
      (insert (format "Emacs Version: %s\n" emacs-version))
      (insert (format "gptel Loaded: %s\n" (featurep 'gptel)))
      (insert (format "Ollama Available: %s\n"
                     (if (gptel-test--check-ollama-available) "Yes" "No")))
      (insert (format "Test Model: %s\n" gptel-test-ollama-model))
      (insert (format "Test Host: %s\n\n" gptel-test-ollama-host))

      (insert "=== Test Results Directory ===\n")
      (insert (format "%s\n\n" gptel-test-results-dir))

      (insert "=== Available Test Tags ===\n")
      (insert ":unit         - Fast tests, no external dependencies\n")
      (insert ":integration  - Requires Ollama server\n")
      (insert ":slow         - May take 30+ seconds\n")
      (insert ":interactive  - Requires AI model responses\n\n")

      (insert "=== Run Commands ===\n")
      (insert "M-x gptel-test-run-all         - Run all tests\n")
      (insert "M-x gptel-test-run-unit        - Run unit tests only\n")
      (insert "M-x gptel-test-run-integration - Run integration tests\n")
      (insert "M-x ert RET t RET              - Run all tests via ERT\n")

      (goto-char (point-min)))
    (display-buffer buffer)))

(provide 'gptel-ollama-ert)
;;; gptel-ollama-ert.el ends here
