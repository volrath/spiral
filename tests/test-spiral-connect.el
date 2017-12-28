;;; test-spiral-connect.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: test-spiral-connect.el
;; Author: Daniel Barreto
;; Copyright (C) 2017 Daniel Barreto
;; Created: Fri Dec  1 10:53:03 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Test spiral connection related functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'buttercup)
(require 'seq)
(require 'with-simulated-input)

(require 'spiral)

(describe "spiral--connection-prompt"
  (before-all
    (spiral-projects-add (spiral-create-project 'localhost:5555 "/foo/bar/" nil nil))
    (spiral-projects-add (spiral-create-project 'localhost:5566 "/root/secret/" nil nil))
    (spiral-projects-add (spiral-create-project '159.98.201.23:60100 nil nil nil)))

  (after-all
    (setq spiral-projects nil))


  (it "understands selecting a project by name"
    (mapc (lambda (input-conn-id-kv)
            (let ((input (car input-conn-id-kv))
                  (conn-id (cdr input-conn-id-kv)))
              (expect
               (with-simulated-input input
                 (spiral--connection-prompt))
               :to-equal (list (spiral-projects-get conn-id) nil))))
          '(("secr TAB RET" . localhost:5566)
            ("bar TAB RET" . localhost:5555)
            ("159 TAB RET" . 159.98.201.23:60100))))


  (it "accepts <host>:<port> values that are not listed in choices"
    (let* ((mocked-proc 'mocked-proc)
           (proc-buffer (get-buffer-create "*proc buffer*")))
      (spy-on 'make-network-process :and-return-value mocked-proc)
      (spy-on 'spiral-socket--get-network-buffer :and-return-value proc-buffer)
      (spy-on 'process-send-string)
      (spy-on 'set-process-sentinel)
      (spy-on 'process-buffer :and-return-value proc-buffer)
      (spy-on 'delete-process)

      (seq-let [new-project new-p] (with-simulated-input "barreto.tech:12345 RET"
                                     (spiral--connection-prompt))
        (expect new-p)
        (expect (spiral-project-id new-project) :to-equal 'barreto.tech:12345)
        (expect (spiral-project-namespace new-project) :to-equal nil)
        (expect (spiral-project-dir new-project) :to-equal nil)
        (expect (spiral-project-socket-repl new-project) :to-equal nil)
        (expect (buffer-name (spiral-project-repl-buffer new-project))
                :to-equal "SPIRAL[barreto.tech:12345]")
        (expect (spiral-project-conn-pool-get-process new-project :client)
                :to-equal mocked-proc))

      (expect 'make-network-process :to-have-been-called-with
              :name "spiral-client"
              :buffer proc-buffer
              :host "barreto.tech"
              :service 12345
              :filter #'spiral-loop-handle-proc-message)
      (expect 'spiral-socket--get-network-buffer :to-have-been-called-with
              :client "barreto.tech" 12345)
      (expect 'process-send-string
              :to-have-been-called-with mocked-proc (spiral-socket--blob))
      (expect 'set-process-sentinel
              :to-have-been-called-with mocked-proc #'spiral-socket--client-sentinel)
      (expect 'process-buffer :to-have-been-called-with mocked-proc)

      (with-current-buffer proc-buffer
        (expect (derived-mode-p 'clojure-mode))
        (expect spiral-conn-id :to-equal 'barreto.tech:12345)
        (expect spiral-loop-process-type :to-equal :client)
        (expect spiral-loop-process-dispatcher :to-equal #'spiral-loop-client-dispatcher))

      (spiral-disconnect'barreto.tech:12345)))


  (it "raises `user-error' if not given something that looks like <host>:<port>"
    (expect (with-simulated-input "somecrazything RET"
              (spiral--connection-prompt))
            :to-throw 'user-error))


  (describe "spiral--connection-prompt-choices"
    (it "when called with project-dir, returns projects that match first, all ordered decr by `:created'"
      (spiral-projects-add (spiral-create-project 'localhost:5544 "/foo/bar/" nil nil))
      (expect (spiral--connection-prompt-choices "/foo/bar/")
              :to-equal
              '("bar [localhost:5544]"
                "bar [localhost:5555]"
                "159.98.201.23:60100"
                "secret [localhost:5566]"))
      (spy-on 'delete-process)
      (spiral-disconnect'localhost:5544))

    (it "when called nil project-dir, returns projects with no dir first"
      (expect (spiral--connection-prompt-choices nil)
              :to-equal
              '("159.98.201.23:60100"
                "secret [localhost:5566]"
                "bar [localhost:5555]")))))



(describe "spiral-connect"
  (it "when asked to connect a non clojure-mode buffer, prompts for confirmation"
    (with-temp-buffer
      (with-simulated-input "n RET"
        (call-interactively #'spiral-connect))))

  (describe "when using it inside a Clojure project"
    :var (real-default-directory
          (project-dir "/random/clj/project/"))

    (before-all
      (spy-on 'spiral-clojure-dir :and-return-value project-dir)
      (setq real-default-directory default-directory)
      (setq default-directory project-dir))

    (after-all
      (setq default-directory real-default-directory))

    (it "If buffer is already connected, warns the user"
      (with-temp-buffer
        (clojure-mode)
        (setq-local spiral-conn-id 'localhost:5555)
        (spy-on 'message)
        (call-interactively #'spiral-connect)

        (expect (spy-calls-count 'message) :to-equal 1)
        (expect (string-match-p "already connected to localhost:5555"
                                (car (spy-calls-args-for 'message 0))))))

    (it "If there's no SPIRAL project for this dir, create a new one automatically"
      (spy-on 'spiral-socket--issue-new-socket-port :and-return-value 12345)
      (spy-on 'start-file-process-shell-command :and-return-value 'mocked-proc)
      (spy-on 'set-process-coding-system)
      (spy-on 'set-process-sentinel)
      (spy-on 'set-process-filter)
      (with-temp-buffer
        (clojure-mode)
        (let ((inhibit-message t))
          (call-interactively #'spiral-connect)))

      (expect 'start-file-process-shell-command :to-have-been-called-times 1)
      (let ((call-args (spy-calls-args-for 'start-file-process-shell-command 0)))
        (expect (car call-args) :to-equal "spiral-socket-server")
        (expect (cl-caddr call-args)
                :to-equal
                (spiral-socket--repl-cmd spiral-default-socket-repl-command)))
      (expect 'set-process-coding-system
              :to-have-been-called-with'mocked-proc 'utf-8-unix 'utf-8-unix)
      (expect 'set-process-sentinel
              :to-have-been-called-with'mocked-proc 'spiral-socket--server-sentinel)
      (expect 'set-process-filter :to-have-been-called-times 1))

    (it "If there a matching SPIRAL project for this dir, automatically connect to it"
      (let ((project (spiral-create-project '127.0.0.1:60189 project-dir nil nil)))
        (spiral-projects-add project)
        (with-temp-buffer
          (clojure-mode)
          (expect (not spiral-mode))
          (expect spiral-conn-id :to-equal nil)

          (let ((inhibit-message t))
            (call-interactively #'spiral-connect))

          (expect spiral-mode)
          (expect spiral-conn-id :to-equal (spiral-project-id project))
          (spiral-disconnect'127.0.0.1:60189)))))


  (describe "when using it outside a Clojure project"
    (it "prompts the user for a Socket REPL HOST:PORT"
      (spy-on 'spiral-clojure-dir :and-return-value nil)
      (spy-on 'spiral--connection-prompt
              :and-return-value (list '((:id . localhost:5555)) nil))
      (with-temp-buffer
        (clojure-mode)
        (let ((inhibit-message t))
          (call-interactively #'spiral-connect))
        (expect 'spiral--connection-prompt :to-have-been-called)))))


(describe "When socket connection gets broken"
  (before-each
    (spiral-projects-add (spiral-create-project 'localhost:5555 "/foo/bar/" nil nil))
    (let ((mocked-buf-name "*temp proc buffer*"))
      (when (get-buffer mocked-buf-name)
        (kill-buffer mocked-buf-name))
      (let ((mocked-proc-buffer (get-buffer-create mocked-buf-name)))
        (with-current-buffer mocked-proc-buffer
          (setq-local spiral-conn-id 'localhost:5555))
        (spy-on 'process-buffer :and-return-value mocked-proc-buffer))))

  (after-all
    (setq spiral-projects nil))

  (it "user gets notified through the REPL"
    (let ((repl-buffer (thread-first 'localhost:5555
                         (spiral-projects-get)
                         (spiral-project-repl-buffer))))
      (spiral-socket--client-sentinel 'mocked-proc "connection broken by peer.")

      (expect (get-buffer-window repl-buffer))
      (with-current-buffer repl-buffer
        (expect (string-match-p
                 "connection broken by peer.$"
                 (buffer-substring-no-properties (point-min) (point-max)))))))

  (it "project is removed from `spiral-projects'"
    (spiral-socket--client-sentinel 'mocked-proc "connection broken by peer.")
    (expect (not spiral-projects))))


(describe "When trying to connect to a wrong authority"
  (it "let the user know in a `spiral-connection-error'"
    (spy-on 'spiral-clojure-dir :and-return-value nil)
    (spy-on 'make-network-process :and-throw-error 'file-error)
    (spy-on 'signal)
    (with-simulated-input "localhost:9999 RET"
      (with-temp-buffer
        (clojure-mode)
        (let ((inhibit-message t))
          (call-interactively #'spiral-connect))))
    (expect 'signal :to-have-been-called-times 1)
    (expect (car (spy-calls-args-for 'signal 0))
            :to-equal
            'spiral-connection-error)))

;;; test-spiral-connect.el ends here
