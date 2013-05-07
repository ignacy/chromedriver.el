## Chromedriver.el - Control Google Chrome browser from Emacs

## Note

This is still a very early version

## Usage:

Add you to your .emacs.d:

    (add-to-list 'load-path "/path/to/chromedriver.el") ;; optional
    (require 'chromedriver)

To allow websocket connections you need to run Chrome with
--remote-debugging-port option. Chromedriver assumes this port is set to 9222

For example on OS X you should close Google Chrome and then execute:

    /Applications/Google Chrome.app/Contents/MacOS/Google Chrome --remote-debugging-port=9222

Chromedriver gives you two commands:
`(connect-to-chrome)` - you should do this before anything else
`(reload-chrome-tab)` - reloads currently connected tab