;;; gnus-o365-oauth2.el -- Perform Office 365 oauth2 authentication for
;;; Gnus based on gnus-gmail-oauth
;;; (https://github.com/ggervasio/gnus-gmail-oauth) and
;;; google-contacts (https://github.com/jd/google-contacts.el)

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Prerequisites:
;; oauth2 tweaked to accept an optional function argument
;; read-authorization-string for the functions that invoke browse-url,
;; see this repo for now.

;;; Commentary:
;;
;; 1. Obtain Office 365 tenant id:
;;
;; See:
;; https://docs.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow
;; https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-v2-protocols#endpoints
;;
;; 2. Set tenant id:
;;
;; (setq gnus-o365-oauth2-tenant-id "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
;;
;; 3. Obtain client identifier:
;;
;; The client-secret may very well not be called for. Ask your
;; Exchange administrator or someone else that has access to the
;; "App registrations experience in the Azure portal", see:
;; https://docs.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow#request-an-authorization-code
;; https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-v2-protocols#app-registration
;; https://aka.ms/appregistrations
;;
;; 4. Set client identifier(s):
;;
;; (setq gnus-o365-oauth2-client-id "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
;; (setq gnus-o365-oauth2-client-secret "xxxx")
;;
;; 5. Set custom authenticator:
;;
;; (require 'oauth2)
;; (require 'gnus-o365-oauth2) ; must be loaded *after* (setq gnus-o365-oauth2-*)
;; (advice-add 'nnimap-login :before-until #'gnus-o365-oauth2-imap-authenticator)
;;
;; 6. Configure plstore (optional):
;; oauth2 uses plstore to save access tokens in ~/emacs.d/oauth2.plstore,
;; which is gpg-encrypted. This allows the gpg passphrase to be cached
;; during the Emacs session:
;;
;; (setq plstore-cache-passphrase-for-symmetric-encryption t)


(defvar gnus-o365-oauth2-tenant-id nil
  "Tenant identifier for the organisation in which you want to access email.")

(defvar gnus-o365-oauth2-client-id nil
  "Office 365 appregistration for your mail app.")

(defvar gnus-o365-oauth2-client-secret nil
  "Client secret, if any.")

(defun gnus-o365-read-authorisation-string ()
  "Ask for the whole nativeclient url from the user and extract
authorisation string."
  (let ((auth-uri
         (read-string "Enter the url you were finally redirected to: ")))
    (when (string-match "[?&]code=\\([^&]*\\)" auth-uri)
      (match-string 1 auth-uri))))

(defvar gnus-o365-oauth2-redirect-uri
  "https://login.microsoftonline.com/common/oauth2/nativeclient"
  "Office 365 oauth2 redirect uri.")

(defconst o365-oauth2-auth-url
  (format "https://login.microsoftonline.com/%s/oauth2/v2.0/authorize"
          gnus-o365-oauth2-tenant-id)
  "Office 365 oauth2 authorisation endpoint.")

(defconst o365-oauth2-token-url
  (format "https://login.microsoftonline.com/%s/oauth2/v2.0/token"
          gnus-o365-oauth2-tenant-id)
  "Office 365 oauth2 token endpoint.")

;;;###autoload
(defun o365-oauth2-auth (resource-url client-id client-secret)
  "Request access to a resource."
  (oauth2-auth
   o365-oauth2-auth-url o365-oauth2-token-url
   client-id client-secret
   resource-url
   nil nil gnus-o365-oauth2-redirect-uri #'gnus-o365-read-authorisation-string))

;;;###autoload
(defun o365-oauth2-auth-and-store (resource-url client-id client-secret)
  "Request access to a Office 365 resource and store it using `auth-source'."
  (oauth2-auth-and-store
   o365-oauth2-auth-url o365-oauth2-token-url
   resource-url
   client-id client-secret
   gnus-o365-oauth2-redirect-uri nil #'gnus-o365-read-authorisation-string))

(defconst gnus-o365-resource-url
  ;; "offline_access" is needed to get a response with a refresh token
  ;; which is critical for the way this module is written.
  "offline_access https://outlook.office.com/mail.read https://outlook.office.com/mail.send"
  "Permissions to request in Office 365, passed to oauth2.el as scope.")

(defun gnus-o365-oauth2-token ()
  "Get oauth2 token for Gnus to access Office 365."
  (let ((token (o365-oauth2-auth-and-store
               gnus-o365-resource-url
               gnus-o365-oauth2-client-id
               gnus-o365-oauth2-client-secret)))
    ;; hack -- always refresh
    (oauth2-refresh-access token)
    token))

(defun gnus-o365-oauth2-imap-authenticator (user password)
  "Authenticator for Office 365 oauth2. Use as :before-until
advice for nnimap-login."
  (if (nnimap-capability "AUTH=XOAUTH2")
      (let ((token (gnus-o365-oauth2-token))
            access-token)
        (setq access-token (oauth2-token-access-token token))
        (if (or (null token)
                (null access-token))
            nil
          (let (sequence challenge)
            (erase-buffer)
            (setq sequence (nnimap-send-command
                            "AUTHENTICATE XOAUTH2 %s"
                            (base64-encode-string
                             (format "user=%s\001auth=Bearer %s\001\001"
                                     (nnimap-quote-specials user)
                                     (nnimap-quote-specials access-token)))))
            (setq challenge (nnimap-wait-for-line "^\\(.*\\)\n"))
            ;; on successful authentication, first line is capabilities,
            ;; next line is response
            (if (string-match (format"^%s OK AUTHENTICATE" sequence) challenge)
                (let (response (nnimap-get-response sequence))
                  (cons t response))
              ;; send empty response on error
              (let (response)
                (erase-buffer)
                (process-send-string
                 (get-buffer-process (current-buffer))
                 "\r\n")
                (setq response (nnimap-get-response sequence))
                (nnheader-report 'nnimap "%s"
                                 (mapconcat (lambda (a)
                                              (format "%s" a))
                                            (car response) " "))
                nil)))))))

(provide 'gnus-o365-oauth2)


;; Local variables:
;; ispell-local-dictionary: "british";
;; end:
