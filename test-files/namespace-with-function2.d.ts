declare namespace isc {
  /**
   *
   *  Method available on the isc object to show a modal prompt to the user.
   *  This method will display the message using the Dialog.Prompt singleton object.
   *  Note: if this prompt is to be shown to the user during some slow JavaScript logic, we
   *  advise calling this method, then using Class.delayCall or Timer.setTimeout
   *  to kick off the slow logic in a separate thread. This ensures that the prompt is showing
   *  before the lengthy execution begins.
   *  Use "&#36;{loadingImage}" to include Canvas.loadingImageSrc.
   *
   *
   *
   * @param {string} message to display
   * @param {Dialog=} additional properties for the Dialog, applied before                    the Dialog is shown
   */
  function showPrompt(message: string, properties?: Dialog): void;

  /**
   *
   *  Clear the modal prompt being shown to the user.
   *
   *
   */
  function clearPrompt(): void;

  /**
   *
   *  Method available on the isc object to show a temporary modal prompt to the user.
   *  This method will display the message using the Dialog.Prompt singleton object, then hide it
   *  using a fade animation effect.
   *  Note: if this prompt is to be shown to the user during some slow JavaScript logic, we
   *  advise calling this method, then using Class.delayCall or Timer.setTimeout
   *  to kick off the slow logic in a separate thread. This ensures that the prompt is showing
   *  before the lengthy execution begins.
   *
   *  The prompt may be cleared before the duration has elapsed via a call to isc.clearPrompt
   *  and any callback specified will still be fired even if the prompt is dismissed early.
   *
   *
   * @param {string} message to display
   * @param {number} how long the message should appear for in milliseconds before  fading from view.
   * @param {Callback=} When the prompt is hidden, callback will be fired.
   * @param {Dialog=} additional properties for the Dialog, applied before                    the Dialog is shown
   */
  function showFadingPrompt(message: string, duration: number, callback?: Callback, properties?: Dialog): void;

  /**
   *  Show a modal dialog with a message, icon, and "OK" button. See Dialog.warnIcon.
   *
   *  The callback will receive boolean true for an OK button click, or null if the Dialog is
   *  dismissed via the close button.
   *
   *
   * @param {string} message to display
   * @param {Callback=} Optional Callback to fire when the user                   dismisses the dialog. This has the single parameter                    'value', indicating the value returned by the Warn                    dialog from 'okClick()' etc.
   * @param {Dialog=} additional properties for the Dialog.                   To set Dialog.buttons for                    the Dialog, set properties.buttons to                    an array of buttons      eg:  { buttons : [Dialog.OK, Dialog.CANCEL] }
   */
  function warn(message: string, callback?: Callback, properties?: Dialog): void;

  /**
   *  Show a modal dialog with a message, icon, and "OK" button. Intended for notifications which
   *  are not really warnings (default icon is less severe). See Dialog.sayIcon.
   *
   *  The callback will receive boolean true for an OK button click, or null if the Dialog is
   *  dismissed via the close button.
   *
   *
   * @param {string} message to display
   * @param {Callback=} Optional Callback to fire when the user                   dismisses the dialog. This has the single parameter                    'value', indicating the value returned by the Warn                    dialog from 'okClick()' etc.
   * @param {Dialog=} additional properties for the Dialog.                   To set Dialog.buttons for                    the Dialog, set properties.buttons to an array of                    buttons      eg:  { buttons : [Dialog.OK, Dialog.CANCEL] }
   */
  function say(message: string, callback?: Callback, properties?: Dialog): void;

  /**
   *  Show a modal dialog with a message, icon, and "Yes" and "No" buttons. See Dialog.askIcon.
   *
   *  The callback will receive boolean true for a Yes button click, boolean false for a No button
   *  click, or null if the Dialog is dismissed via the close button.
   *
   *
   * @param {string} message to display
   * @param {Callback=} Callback to fire when the                   user clicks a button to dismiss the dialog.                    This has the single parameter 'value', indicating the                    value returned by the Warn dialog from 'okClick()' etc.
   * @param {Dialog=} additional properties for the Dialog.                   To set Dialog.buttons for                    the Dialog, set properties.buttons to an array                    of buttons      eg:  { buttons : [Dialog.OK, Dialog.CANCEL] }
   */
  function ask(message: string, callback?: Callback, properties?: Dialog): void;

  /**
   *  Show a modal dialog with a message, icon, and "OK" and "Cancel" buttons. See Dialog.confirmIcon.
   *
   *  The callback will receive boolean true for an OK button click, or null for a Cancel click or
   *  if the Dialog is dismissed via the close button.
   *
   *  Note: this does not override the native window.confirm() method.
   *
   *
   * @param {string} message to display
   * @param {Callback=} Callback to fire when the                   user clicks a button to dismiss the dialog.                    This has the single parameter 'value', indicating the                    value returned by the Warn dialog from 'okClick()' etc.
   * @param {Dialog=} additional properties for the Dialog.                   To set Dialog.buttons for                    the Dialog, set properties.buttons to an array of                    buttons      eg:  { buttons : [Dialog.OK, Dialog.CANCEL] }
   */
  function confirm(message: string, callback?: Callback, properties?: Dialog): void;

  /**
   *  Show a modal dialog with a text entry box, asking the user to enter a value.
   *
   *  As with other convenience methods that show Dialogs, such as isc.warn,
   *  the dialog is shown and the function immediately returns. When the user responds, the
   *  provided callback is called.
   *
   *  If the user clicks OK, the value typed in is passed to the callback (including the empty
   *  string ("") if nothing was entered. If the user clicks cancel, the value passed to the
   *  callback is null.
   *
   *  A default value for the text field can be passed via properties.defaultValue.
   *
   *  Keyboard focus is automatically placed in the text entry field, and hitting the enter key is
   *  the equivalent of pressing OK.
   *
   *
   * @param {string} message to display
   * @param {Callback=} Callback to fire when the                   user clicks a button to dismiss the dialog.                    This has the single parameter 'value', indicating the                    user entry, or null if cancel was pressed or the window                    closed
   * @param {Dialog=} additional properties for the Dialog.                   To set Dialog.buttons for                    the Dialog, set properties.buttons to an array of                    buttons      eg:  { buttons : [Dialog.OK, Dialog.CANCEL] }
   */
  function askForValue(message: string, callback?: Callback, properties?: Dialog): void;

  /**
   *  If a dialog triggered via isc.say, isc.ask,
   *  isc.warn, isc.confirm or
   *  isc.askForValue is currently visible, it will be dismissed. The
   *  callback passed to the relevant method will never fire.
   *
   *  Note this is a rarely used API with very few valid use cases. As an example, perhaps some kind of
   *  periodic (non-user triggered) event would cause an entire area of the UI to be removed (such as a tab)
   *  and the system wants to ensure that no modal dialogs are currently showing from that part of the UI.
   *  In this case, while dismissCurrentDialog could be used to ensure the part of the UI being
   *  removed didn't leave behind a modal dialog.
   *
   *  To clear a modal prompt shown by isc.showPrompt, use isc.clearPrompt instead.
   *
   *
   */
  function dismissCurrentDialog(): void;

  /**
   *  Handle a complete login interaction with a typical login dialog asking for username and
   *  password credentials using the LoginDialog class.
   *
   *  As with other convenience methods that show Dialogs, such as isc.warn,
   *  the dialog is shown and the function immediately returns. When the user responds, the
   *  provided callback function is called.
   *
   *  If the user clicks the "Log in" button, the credentials entered by the user are passed to
   *  the provided "loginFunc" as an Object with properties "username" and "password" (NOTE: both
   *  property names are all lowercase), as the variable "credentials". For example:
   *  { username: "barney", password: "rUbbL3" }
   *
   *  The "loginFunc" should then attempt to log in by whatever means is necessary. The second
   *  parameter to the loginFunc, "dialogCallback", is a function, which must be called whether
   *  login succeeds or fails with a true/false value indicating whether login succeeded.
   *
   *  If the login dialog is dismissable (settable as properties.dismissable, default false) and
   *  the user dismisses it, the loginFunc will be fired with null for the credentials.
   *
   *  The following code shows typical usage. This code assumes you have created a global
   *  function sendCredentials() that send credentials to some authentication system and fires a
   *  callback function with the result:
   *
   *  isc.showLoginDialog(function (credentials, dialogCallback) {
   *    if (credentials == null) return; // dismissed
   *
   *    // send credentials
   *    sendCredentials(credentials, function (loginSucceeded) {
   *      // report success or failure
   *      dialogCallback(loginSucceeded);
   *    })
   *  })
   *
   *  The login dialog has several built-in behaviors:
   *
   *
   * - keyboard focus is automatically placed in the username field
   *
   * - hitting enter in the username field proceeds to the password field
   *
   * - hitting enter in the password field submits (fires the provided callback)
   *
   *  In addition to normal properties supported by Dialog/Window, the following special
   *  properties can be passed:
   *
   *
   * - username: initial value for the username field
   *
   * - password: initial value for the password field
   *
   * - usernameTitle: title for the username field
   *
   * - passwordTitle: title for the password field
   *
   * - errorMessage: default error message on login failure
   *
   * - loginButtonTitle: title for the login button
   *
   * - dismissable: whether the dialog can be dismissed, default false
   *
   * - errorStyle: CSS style for the error message, if shown
   *
   *  See below for links to the default values for these properties.
   *
   *
   * @param {Callback} Function to call to attempt login. Receives parameters                   "credentials" and "dialogCallback", described above
   * @param {LoginDialog=} additional properties for the Dialog
   */
  function showLoginDialog(loginFunc: Callback, properties?: LoginDialog): void;

  /**
   *  Same as Log.logWarn.
   *
   *
   * @param {string} message to log
   * @param {string=} category to log in, defaults to "Log"
   */
  function logWarn(message: string, category?: string): void;

  /**
   *  Same as Log.echo.
   *
   *
   * @param {any} object to echo
   */
  function echo(value: any): string;

  /**
   *  Same as Log.echoAll.
   *
   *
   * @param {any} object to echo
   */
  function echoAll(value: any): string;

  /**
   *  Same as Log.echoLeaf.
   *
   *
   * @param {any} object to echo
   */
  function echoLeaf(value: any): string;

  /**
   *  Logs the echoed object (using isc.echo) as a warning, prefixed with an
   *  optional message.
   *
   *
   * @param {any} object to echo
   * @param {string} message to log
   */
  function logEcho(value: any, message: string): void;

  /**
   *  Logs the echoed object (using isc.echoAll) as a warning, prefixed with an
   *  optional message.
   *
   *
   * @param {any} object to echo
   * @param {string} message to log
   */
  function logEchoAll(value: any, message: string): void;

  /**
   *
   *  Add all properties and methods from any number of objects to a destination object,
   *  overwriting properties in the destination object.
   *
   *  Common uses of addProperties include creating a shallow copy of an object:
   *
   *    isc.addProperties({}, someObject);
   *
   *  Combining settings in order of precedence:
   *
   *    isc.addProperties({}, defaults, overrides, skinOverrides);
   *
   *
   *
   *  NOTES:
   *
   * - Do not use addProperties to add defaults to an ISC class.
   *  Use Class.addProperties, as in:
   *  MyClassName.addProperties().
   *
   * - You may find it more convenient to use the instance method Class.addProperties,
   *  as in: myClassInstance.addProperties(), but there's no functional
   *  difference from using this method.
   *
   *
   *
   * @param {object} object to add properties to
   * @param {object=} objects to obtain properties from. Properties of all      arguments other than destination are applied in turn.
   */
  function addProperties(destination: object, ...args: any[]): object;
}