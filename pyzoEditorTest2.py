import sys

_path = r"C:\Users\tyler\src\GitHub\InProgress\pyzo"

if _path not in sys.path:
    sys.path.insert(0, _path)

import os
import re
import time
import json
import codecs
from Qt import QtCore, QtGui, QtWidgets
from pyzo.codeeditor import CodeEditor
import pyzo.codeeditor.parsers.tokens as Tokens
from pyzo.util._locale import translate
from pyzo.core.codeparser import Parser


# For accessing the config stuff.
class DotProxy(object):
    def __init__(self, obj):
        self.obj = obj

    def __contains__(self, key):
        return key in obj.keys()

    def __setitem__(self, key, val):
        self.obj[key] = val

    def __getitem__(self, key):
        return self.wrap(self.obj[key])

    def __getattr__(self, key):
        try:
            return self.wrap(getattr(self.obj, key))
        except AttributeError:
            try:
                return self.wrap(self[key])
            except KeyError:
                # For now, dot-accessible dicts default to None if the key isn't found
                # In the future I may want to `raise AttributeError(key)`
                return None

    @classmethod
    def wrap(cls, value):
        if isinstance(value, dict):
            return cls(value)
        return value


def normalizePath(path):
    """Normalize the path given.
    All slashes will be made the same (and doubles removed)
    The real case as stored on the file system is recovered.
    Returns None on error.
    """

    # normalize
    path = os.path.abspath(path)  # make sure it is defined from the drive up
    path = os.path.normpath(path)

    # If does not exist, return as is.
    # This also happens if the path's case is incorrect and the
    # file system is case sensitive. That's ok, because the stuff we
    # do below is intended to get the path right on case insensitive
    # file systems.
    if not os.path.isfile(path):
        return path

    # split drive name from the rest
    drive, rest = os.path.splitdrive(path)
    fullpath = drive.upper() + os.sep

    # make lowercase and split in parts
    parts = rest.lower().split(os.sep)
    parts = [part for part in parts if part]

    for part in parts:
        options = [x for x in os.listdir(fullpath) if x.lower() == part]
        if len(options) > 1:
            print("Error normalizing path: Ambiguous path names!")
            return path
        elif not options:
            print("Invalid path (part %s) in %s" % (part, fullpath))
            return path
        fullpath = os.path.join(fullpath, options[0])

    # remove last sep
    return fullpath


def parseLine_autocomplete(tokens):
    """Given a list of tokens (from start to cursor position)
    returns a tuple (base, name).
    autocomp_parse("eat = banan") -> "", "banan"
      ...("eat = food.fruit.ban") -> "food.fruit", "ban"
    When no match found, both elements are an empty string.
    """
    if not len(tokens):
        return "", ""

    if isinstance(tokens[-1], Tokens.NonIdentifierToken) and str(tokens[-1]) == ".":
        name = ""
    elif isinstance(tokens[-1], (Tokens.IdentifierToken, Tokens.KeywordToken)):
        name = str(tokens[-1])
    else:
        return "", ""

    needle = ""
    # Now go through the remaining tokens in reverse order
    for token in tokens[-2::-1]:
        if isinstance(token, Tokens.NonIdentifierToken) and str(token) == ".":
            needle = str(token) + needle
        elif isinstance(token, (Tokens.IdentifierToken, Tokens.KeywordToken)):
            needle = str(token) + needle
        else:
            break

    if needle.endswith("."):
        needle = needle[:-1]

    return needle, name


def parseLine_signature(tokens):
    """Given a list of tokens (from start to cursor position)
    returns a tuple (name, needle, stats).
    stats is another tuple:
    - location of end bracket
    - amount of kommas till cursor (taking nested brackets into account)
    """

    openBraces = []  # Positions at which braces are opened
    for token in tokens:
        if not isinstance(
            token,
            (Tokens.NonIdentifierToken, Tokens.OpenParenToken, Tokens.CloseParenToken),
        ):
            continue
        for i, c in enumerate(str(token)):
            if c == "(":
                openBraces.append(token.start + i)
            elif c == ")":
                if len(openBraces):
                    openBraces.pop()

    if len(openBraces):
        i = openBraces[-1]
        # Now trim the token list up to (but not inculding) position of openBraces
        tokens = list(filter(lambda token: token.start < i, tokens))

        # Trim the last token
        if len(tokens):
            tokens[-1].end = i

        name, needle = parseLine_autocomplete(tokens)
        return name, needle, (i, 0)  # TODO: implement stats

    else:
        return "", "", (0, 0)


def determineLineEnding(text):
    """Get the line ending style used in the text.
    \n, \r, \r\n,
    The EOLmode is determined by counting the occurrences of each
    line ending...
    """
    text = text[:32768]  # Limit search for large files

    # test line ending by counting the occurrence of each
    c_win = text.count("\r\n")
    c_mac = text.count("\r") - c_win
    c_lin = text.count("\n") - c_win
    # set the appropriate style
    if c_win > c_mac and c_win > c_lin:
        mode = "\r\n"
    elif c_mac > c_win and c_mac > c_lin:
        mode = "\r"
    else:
        mode = "\n"

    # return
    return mode


def loadIcons():
    """loadIcons()
    Load all icons in the icon dir.
    """
    # Get directory containing the icons
    iconDir = r"C:\Users\tyler\src\GitHub\InProgress\pyzo\pyzo\resources\icons"
    ret = DotProxy({})
    # Construct other icons
    for fname in os.listdir(iconDir):
        if fname.endswith(".png"):
            # Short and full name
            name = fname.split(".")[0]
            name = name.replace("pyzo_", "")  # discart prefix
            ffname = os.path.join(iconDir, fname)
            # Create icon
            icon = QtGui.QIcon()
            icon.addFile(ffname, QtCore.QSize(16, 16))
            # Store
            ret[name] = icon
    return ret


class KeyMapper(QtCore.QObject):
    """
    This class is accessable via keyMapper
    keyMapper.keyMappingChanged is emitted when keybindings are changed
    """

    keyMappingChanged = QtCore.Signal()

    def __init__(self, config, parent):
        super(KeyMapper, self).__init__(parent=parent)
        self._config = config

    def setShortcut(self, action):
        """
        When an action is created or when keymappings are changed, this method
        is called to set the shortcut of an action based on its menuPath
        (which is the key in self._config.shortcuts2, e.g. shell__clear_screen)
        """
        if action.menuPath in self._config.shortcuts2.keys():
            # Set shortcut so Qt can do its magic
            shortcuts = self._config.shortcuts2[action.menuPath]
            action.setShortcuts(shortcuts.split(","))
            # issue #470, http://stackoverflow.com/questions/23916623
            self.parent().addAction(action)
            # Also store shortcut text (used in display of tooltip
            shortcuts = shortcuts.replace(",", ", ").replace("  ", " ")
            action._shortcutsText = shortcuts.rstrip(", ")


class CallTipObject:
    """Object to help the process of call tips.
    An instance of this class is created for each call tip action.
    """

    def __init__(self, textCtrl, name, offset):
        self.textCtrl = textCtrl
        self.name = name
        self.bufferName = name
        self.offset = offset

    def tryUsingBuffer(self):
        """tryUsingBuffer()
        Try performing this callTip using the buffer.
        Returns True on success.
        """
        bufferName = self.textCtrl._callTipBuffer_name
        t = time.time() - self.textCtrl._callTipBuffer_time
        if self.bufferName == bufferName and t < 0:
            self._finish(self.textCtrl._callTipBuffer_result)
            return True
        else:
            return False

    def finish(self, callTipText):
        """finish(callTipText)
        Finish the introspection using the given calltipText.
        Will also automatically call setBuffer.
        """
        self.setBuffer(callTipText)
        self._finish(callTipText)

    def setBuffer(self, callTipText, timeout=4):
        """setBuffer(callTipText)
        Sets the buffer with the provided text."""
        self.textCtrl._callTipBuffer_name = self.bufferName
        self.textCtrl._callTipBuffer_time = time.time() + timeout
        self.textCtrl._callTipBuffer_result = callTipText

    def _finish(self, callTipText):
        self.textCtrl.calltipShow(self.offset, callTipText, True)


class AutoCompObject:
    """Object to help the process of auto completion.
    An instance of this class is created for each auto completion action.
    """

    def __init__(self, textCtrl, name, needle):
        self.textCtrl = textCtrl
        self.bufferName = name  # name to identify with
        self.name = name  # object to find attributes of
        self.needle = needle  # partial name to look for
        self.names = set()  # the names (use a set to prevent duplicates)
        self.importNames = []
        self.importLines = {}

    def addNames(self, names):
        """addNames(names)
        Add a list of names to the collection.
        Duplicates are removed."""
        self.names.update(names)

    def tryUsingBuffer(self):
        """tryUsingBuffer()
        Try performing this auto-completion using the buffer.
        Returns True on success.
        """
        bufferName = self.textCtrl._autoCompBuffer_name
        t = time.time() - self.textCtrl._autoCompBuffer_time
        if self.bufferName == bufferName and t < 0:
            self._finish(self.textCtrl._autoCompBuffer_result)
            return True
        else:
            return False

    def finish(self):
        """finish()
        Finish the introspection using the collected names.
        Will automatically call setBuffer.
        """
        # Remember at the object that started this introspection
        # and get sorted names
        names = self.setBuffer(self.names)
        # really finish
        self._finish(names)

    def setBuffer(self, names=None, timeout=None):
        """setBuffer(names=None)
        Sets the buffer with the provided names (or the collected names).
        Also returns a list with the sorted names."""
        # Determine timeout
        # Global namespaces change more often than local one, plus when
        # typing a xxx.yyy, the autocompletion buffer changes and is thus
        # automatically refreshed.
        # I've once encountered a wrong autocomp list on an object, but
        # haven' been able to reproduce it. It was probably some odity.
        if timeout is None:
            if self.bufferName:
                timeout = 5
            else:
                timeout = 1
        # Get names
        if names is None:
            names = self.names
        # Make list and sort
        names = list(names)
        names.sort(key=str.upper)
        # Store
        self.textCtrl._autoCompBuffer_name = self.bufferName
        self.textCtrl._autoCompBuffer_time = time.time() + timeout
        self.textCtrl._autoCompBuffer_result = names
        # Return sorted list
        return names

    def _finish(self, names):
        # Show completion list if required.
        self.textCtrl.autocompleteShow(len(self.needle), names, self.name != "")

    def nameInImportNames(self, importNames):
        """nameInImportNames(importNames)
        Test whether the name, or a base part of it is present in the
        given list of names. Returns the (part of) the name that's in
        the list, or None otherwise.
        """
        baseName = self.name
        while baseName not in importNames:
            if "." in baseName:
                baseName = baseName.rsplit(".", 1)[0]
            else:
                baseName = None
                break
        return baseName


class Menu(QtWidgets.QMenu):
    """Menu(parent=None, name=None)

    Base class for all menus. Has methods to add actions of all sorts.

    The add* methods all have the name and icon as first two arguments.
    This is not so consistent with the Qt API for addAction, but it allows
    for cleaner code to add items; the first item can be quite long because
    it is a translation. In the current API, the second and subsequent
    arguments usually fit nicely on the second line.

    """

    def __init__(self, parent=None, name=None):
        super(Menu, self).__init__(parent)

        par = self.parent()
        self._keymapper = par._keymapper
        self._icons = par._icons

        # Make sure that the menu has a title
        if name:
            self.setTitle(name)
        else:
            raise ValueError

        try:
            self.setToolTipsVisible(True)
        except Exception:
            pass
        # Set tooltip too.
        if hasattr(name, "tt"):
            self.setStatusTip(name.tt)
            self.setToolTip(name.tt)

        # Action groups within the menu keep track of the selected value
        self._groups = {}

        # menuPath is used to bind shortcuts, it is ,e.g. shell__clear_screen
        if hasattr(parent, "menuPath"):
            self.menuPath = parent.menuPath + "__"
        else:
            self.menuPath = ""  # This is a top-level menu

        # Get key for this menu
        key = name
        if hasattr(name, "key"):
            key = name.key
        self.menuPath += self._createMenuPathName(key)

        # Build the menu. Happens only once
        self.build()

    def _createMenuPathName(self, name):
        """
        Convert a menu title into a menuPath component name
        e.g. Interrupt current shell -> interrupt_current_shell
        """
        # hide anything between brackets
        name = re.sub(r"\(.*\)", "", name)
        # replace invalid chars
        name = name.replace(" ", "_")
        if name and name[0] in "0123456789_":
            name = "_" + name
        name = re.sub("[^a-zA-z_0-9]", "", name)
        return name.lower()

    def _addAction(self, text, icon, selected=None):
        """Convenience function that makes the right call to addAction()."""

        # Add the action
        if icon is None:
            a = self.addAction(text)
        else:
            a = self.addAction(icon, text)

        # Checkable?
        if selected is not None:
            a.setCheckable(True)
            a.setChecked(selected)

        # Set tooltip if we can find it
        if hasattr(text, "tt"):
            a.setStatusTip(text.tt)
            a.setToolTip(text.tt)

        # Find the key (untranslated name) for this menu item
        key = a.text()
        if hasattr(text, "key"):
            key = text.key
        a.menuPath = self.menuPath + "__" + self._createMenuPathName(key)

        # Register the action so its keymap is kept up to date
        self._keymapper.keyMappingChanged.connect(
            lambda: self._keymapper.setShortcut(a)
        )
        self._keymapper.setShortcut(a)

        return a

    def build(self):
        """
        Add all actions to the menu. To be overridden.
        """
        pass

    def popup(self, pos, action=None):
        self._pos = pos
        super().popup(pos, action)

    def addMenu(self, menu, icon=None):
        """
        Add a (sub)menu to this menu.
        """

        # Add menu in the conventional way
        a = QtWidgets.QMenu.addMenu(self, menu)
        a.menuPath = menu.menuPath

        # Set icon
        if icon is not None:
            a.setIcon(icon)

        return menu

    def addItem(self, text, icon=None, callback=None, value=None):
        """
        Add an item to the menu. If callback is given and not None,
        connect triggered signal to the callback. If value is None or not
        given, callback is called without parameteres, otherwise it is called
        with value as parameter
        """

        # Add action
        a = self._addAction(text, icon)

        # Connect the menu item to its callback
        if callback:
            if value is not None:
                a.triggered.connect(lambda b=None, v=value: callback(v))
            else:
                a.triggered.connect(lambda b=None: callback())

        return a

    def addGroupItem(self, text, icon=None, callback=None, value=None, group=None):
        """
        Add a 'select-one' option to the menu. Items with equal group value form
        a group. If callback is specified and not None, the callback is called
        for the new active item, with the value for that item as parameter
        whenever the selection is changed
        """

        # Init action
        a = self._addAction(text, icon)
        a.setCheckable(True)

        # Connect the menu item to its callback (toggled is a signal only
        # emitted by checkable actions, and can also be called programmatically,
        # e.g. in QActionGroup)
        if callback:

            def doCallback(b, v):
                if b:
                    callback(v)

            a.toggled.connect(lambda b=None, v=value: doCallback(a.isChecked(), v))

        # Add the menu item to a action group
        if group is None:
            group = "default"
        if group not in self._groups:
            # self._groups contains tuples (actiongroup, dict-of-actions)
            self._groups[group] = (QtWidgets.QActionGroup(self), {})

        actionGroup, actions = self._groups[group]
        actionGroup.addAction(a)
        actions[value] = a

        return a

    def addCheckItem(self, text, icon=None, callback=None, value=None, selected=False):
        """
        Add a true/false item to the menu. If callback is specified and not
        None, the callback is called when the item is changed. If value is not
        specified or None, callback is called with the new state as parameter.
        Otherwise, it is called with the new state and value as parameters
        """

        # Add action
        a = self._addAction(text, icon, selected)

        # Connect the menu item to its callback
        if callback:
            if value is not None:
                a.triggered.connect(lambda b=None, v=value: callback(a.isChecked(), v))
            else:
                a.triggered.connect(lambda b=None: callback(a.isChecked()))

        return a

    def setCheckedOption(self, group, value):
        """
        Set the selected value of a group. This will also activate the
        callback function of the item that gets selected.
        if group is None the default group is used.
        """
        if group is None:
            group = "default"
        actionGroup, actions = self._groups[group]
        if value in actions:
            actions[value].setChecked(True)


class EditorContextMenu(Menu):
    """This is the context menu for the editor"""

    def __init__(self, editor, window, name="EditorContextMenu"):
        self._editor = editor
        self._window = window
        super(EditorContextMenu, self).__init__(editor, name)

    def build(self):
        """Build menu"""

        """
        self.addItem(
            translate(
                "menu",
                "Help on this expression ::: Show help for the selected expression.",
            ),
            self._icons.help,
            self._editItemCallback,
            "helpOnText",
        )

        self.addSeparator()

        # This is a subset of the edit menu. Copied manually.
        self.addItem(
            translate("menu", "Cut ::: Cut the selected text."),
            self._icons.cut,
            self._editItemCallback,
            "cut",
        )
        self.addItem(
            translate("menu", "Copy ::: Copy the selected text to the clipboard."),
            self._icons.page_white_copy,
            self._editItemCallback,
            "copy",
        )
        self.addItem(
            translate("menu", "Paste ::: Paste the text that is now on the clipboard."),
            self._icons.paste_plain,
            self._editItemCallback,
            "paste",
        )
        self.addItem(
            translate("menu", "Select all ::: Select all text."),
            self._icons.sum,
            self._editItemCallback,
            "selectAll",
        )
        self.addSeparator()
        self.addItem(
            translate("menu", "Indent ::: Indent the selected line."),
            self._icons.text_indent,
            self._editItemCallback,
            "indentSelection",
        )
        self.addItem(
            translate("menu", "Dedent ::: Unindent the selected line."),
            self._icons.text_indent_remove,
            self._editItemCallback,
            "dedentSelection",
        )
        self.addItem(
            translate("menu", "Comment ::: Comment the selected line."),
            self._icons.comment_add,
            self._editItemCallback,
            "commentCode",
        )
        self.addItem(
            translate("menu", "Uncomment ::: Uncomment the selected line."),
            self._icons.comment_delete,
            self._editItemCallback,
            "uncommentCode",
        )
        """
        self.addItem(
            translate(
                "menu", "Toggle Comment ::: Toggle comment for the selected line."
            ),
            None,
            self._editItemCallback,
            "toggleCommentCode",
        )
        self.addItem(
            translate(
                "menu",
                "Justify comment/docstring::: Reshape the selected text so it is aligned to around 70 characters.",
            ),
            self._icons.text_align_justify,
            self._editItemCallback,
            "justifyText",
        )
        self.addSeparator()
        self.addItem(
            translate(
                "menu", "Goto Definition ::: Go to definition of word under cursor."
            ),
            self._icons.debug_return,
            self._editItemCallback,
            "gotoDef",
        )
        self.addItem(
            translate("menu", "Open directory in file browser"),
            None,
            self._editItemCallback,
            "opendir",
        )

        """
        self.addSeparator()
        self.addItem(
            translate(
                "menu",
                "Find or replace ::: Show find/replace widget. Initialize with selected text.",
            ),
            self._icons.find,
            pyzo.editors._findReplace.startFind,
        )
        self.addItem(
            translate(
                "menu",
                "Find selection ::: Find the next occurrence of the selected text.",
            ),
            None,
            pyzo.editors._findReplace.findSelection,
        )
        self.addItem(
            translate(
                "menu",
                "Find selection backward ::: Find the previous occurrence of the selected text.",
            ),
            None,
            pyzo.editors._findReplace.findSelectionBw,
        )
        """

        # This is a subset of the run menu. Copied manually.
        self.addSeparator()
        self.addItem(
            translate(
                "menu",
                "Run selection ::: Run the current editor's selected lines, selected words on the current line, or current line if there is no selection.",
            ),  # noqa
            self._icons.run_lines,
            self._runSelected,
        )

    def _editItemCallback(self, action):
        # If the widget has a 'name' attribute, call it
        if action == "opendir":
            return
            """
            fileBrowser = pyzo.toolManager.getTool("pyzofilebrowser")
            if fileBrowser:
                fileBrowser.setPath(os.path.dirname(self._editor.filename))
            """
        elif action == "helpOnText":
            self._editor.helpOnText(self._pos)
        else:
            getattr(self._editor, action)()

    def _runSelected(self):
        runMenu = self._window.menuBar()._menumap["run"]
        runMenu._runSelected()


class BaseTextCtrl(CodeEditor):
    """The base text control class
    Inherited by the shell class and the Pyzo editor.
    The class implements autocompletion, calltips, and auto-help
    """

    def __init__(self, config, *args, **kwds):
        super().__init__(*args, **kwds)
        self._config = config

        # Set font and zooming
        self.setFont(self._config.view.fontname)
        self.setZoom(self._config.view.zoom)
        self.setShowWhitespace(self._config.view.showWhitespace)
        self.setHighlightMatchingBracket(self._config.view.highlightMatchingBracket)

        # Create timer for autocompletion delay
        self._delayTimer = QtCore.QTimer(self)
        self._delayTimer.setSingleShot(True)
        self._delayTimer.timeout.connect(self._introspectNow)

        # For buffering autocompletion and calltip info
        self._callTipBuffer_name = ""
        self._callTipBuffer_time = 0
        self._callTipBuffer_result = ""
        self._autoCompBuffer_name = ""
        self._autoCompBuffer_time = 0
        self._autoCompBuffer_result = []

        self.setAutoCompletionAcceptKeysFromStr(
            self._config.settings.autoComplete_acceptKeys
        )

        self.completer().highlighted.connect(self.updateHelp)
        self.setIndentUsingSpaces(self._config.settings.defaultIndentUsingSpaces)
        self.setIndentWidth(self._config.settings.defaultIndentWidth)
        self.setAutocompletPopupSize(*self._config.view.autoComplete_popupSize)
        self.setAutocompleteMinChars(self._config.settings.autoComplete_minChars)
        self.setCancelCallback(self.restoreHelp)

    def setAutoCompletionAcceptKeysFromStr(self, keys):
        """Set the keys that can accept an autocompletion from a comma delimited string."""
        # Set autocomp accept key to default if necessary.
        # We force it to be string (see issue 134)
        if not isinstance(keys, str):
            keys = "Tab"
        # Split
        keys = keys.replace(",", " ").split(" ")
        keys = [key for key in keys if key]
        # Set autocomp accept keys
        qtKeys = []
        for key in keys:
            if len(key) > 1:
                key = "Key_" + key[0].upper() + key[1:].lower()
                qtkey = getattr(QtCore.Qt, key, None)
            else:
                qtkey = ord(key)
            if qtkey:
                qtKeys.append(qtkey)

        if QtCore.Qt.Key_Enter in qtKeys and QtCore.Qt.Key_Return not in qtKeys:
            qtKeys.append(QtCore.Qt.Key_Return)
        self.setAutoCompletionAcceptKeys(*qtKeys)

    def _isValidPython(self):
        """_isValidPython()
        Check if the code at the cursor is valid python:
        - the active lexer is the python lexer
        - the style at the cursor is "default"
        """
        # TODO:
        return True

    def getTokensUpToCursor(self, cursor):
        # In order to find the tokens, we need the userState from the highlighter
        if cursor.block().previous().isValid():
            previousState = cursor.block().previous().userState()
        else:
            previousState = 0

        text = cursor.block().text()[: cursor.positionInBlock()]

        return (
            text,
            [
                t for t in self.parser().parseLine(text, previousState) if t.isToken
            ],  # filter to remove BlockStates
        )

    def introspect(self, tryAutoComp=False, delay=True):
        """introspect(tryAutoComp=False, delay=True)

        The starting point for introspection (autocompletion and calltip).
        It will always try to produce a calltip. If tryAutoComp is True,
        will also try to produce an autocompletion list (which, on success,
        will hide the calltip).

        This method will obtain the line and (re)start a timer that will
        call _introspectNow() after a short while. This way, if the
        user types a lot of characters, there is not a stream of useless
        introspection attempts; the introspection is only really started
        after he stops typing for, say 0.1 or 0.5 seconds (depending on
        self._config.autoCompDelay).

        The method _introspectNow() will parse the line to obtain
        information required to obtain the autocompletion and signature
        information. Then it calls processCallTip and processAutoComp
        which are implemented in the editor and shell classes.
        """

        # Find the tokens up to the cursor
        cursor = self.textCursor()

        text, tokensUptoCursor = self.getTokensUpToCursor(cursor)

        # TODO: Only proceed if valid python (no need to check for comments/
        # strings, this is done by the processing of the tokens). Check for python style

        # Is the char valid for auto completion?
        if tryAutoComp:
            if not text or not (text[-1] in (Tokens.ALPHANUM + "._")):
                self.autocompleteCancel()
                tryAutoComp = False

        # Store line and (re)start timer
        cursor.setKeepPositionOnInsert(True)
        self._delayTimer._tokensUptoCursor = tokensUptoCursor
        self._delayTimer._cursor = cursor
        self._delayTimer._tryAutoComp = tryAutoComp
        if delay:
            self._delayTimer.start(self._config.advanced.autoCompDelay)
        else:
            self._delayTimer.start(1)  # self._introspectNow()

    def _introspectNow(self):
        """This method is called a short while after introspect()
        by the timer. It parses the line and calls the specific methods
        to process the callTip and autoComp.
        """

        tokens = self._delayTimer._tokensUptoCursor

        if self._config.settings.autoCallTip:
            # Parse the line, to get the name of the function we should calltip
            # if the name is empty/None, we should not show a signature
            name, needle, stats = parseLine_signature(tokens)

            if needle:
                # Compose actual name
                fullName = needle
                if name:
                    fullName = name + "." + needle
                # Process
                offset = (
                    self._delayTimer._cursor.positionInBlock() - stats[0] + len(needle)
                )
                cto = CallTipObject(self, fullName, offset)
                self.processCallTip(cto)
            else:
                self.calltipCancel()

        if self._delayTimer._tryAutoComp and self._config.settings.autoComplete:
            # Parse the line, to see what (partial) name we need to complete
            name, needle = parseLine_autocomplete(tokens)

            if name or needle:
                # Try to do auto completion
                aco = AutoCompObject(self, name, needle)
                self.processAutoComp(aco)

    def processCallTip(self, cto):
        """Overridden in derive class"""
        pass

    def processAutoComp(self, aco):
        """Overridden in derive class"""
        pass

    def _onDoubleClick(self):
        """When double clicking on a name, autocomplete it."""
        self.processHelp(addToHist=True)

    def helpOnText(self, pos):
        return
        """
        hw = pyzo.toolManager.getTool("pyzointeractivehelp")
        if not hw:
            return
        name = self.textCursor().selectedText().strip()
        if name == "":
            cursor = self.cursorForPosition(pos - self.mapToGlobal(QtCore.QPoint(0, 0)))
            line = cursor.block().text()
            limit = cursor.positionInBlock()
            while limit < len(line) and (
                line[limit].isalnum() or line[limit] in (".", "_")
            ):
                limit += 1
                cursor.movePosition(cursor.Right)
            _, tokens = self.getTokensUpToCursor(cursor)
            nameBefore, name = parseLine_autocomplete(tokens)
            if nameBefore:
                name = "%s.%s" % (nameBefore, name)
        if name != "":
            hw.setObjectName(name, True)
        """

    def processHelp(self, name=None, showError=False, addToHist=False):
        """Show help on the given full object name.
        - called when going up/down in the autocompletion list.
        - called when double clicking a name
        """
        # uses parse_autocomplete() to find baseName and objectName
        return
        """
        # Get help tool
        hw = pyzo.toolManager.getTool("pyzointeractivehelp")
        ass = pyzo.toolManager.getTool("pyzoassistant")
        # Get the shell
        shell = pyzo.shells.getCurrentShell()
        # Both should exist
        if not hw or not shell:
            return

        if not name:
            # Obtain name from current cursor position

            # Is this valid python?
            if self._isValidPython():
                # Obtain line from text
                cursor = self.textCursor()
                line = cursor.block().text()
                text = line[: cursor.positionInBlock()]
                # Obtain
                nameBefore, name = parseLine_autocomplete(text)
                if nameBefore:
                    name = "%s.%s" % (nameBefore, name)

        if name:
            hw.helpFromCompletion(name, addToHist)
        if ass:
            ass.showHelpForTerm(name)
        """

    ## Callbacks
    def updateHelp(self, name):
        """A name has been highlighted, show help on that name"""

        if self._autoCompBuffer_name:
            name = self._autoCompBuffer_name + "." + name
        elif not self.completer().completionPrefix():
            # Dont update help if there is no dot or prefix;
            # the choice would be arbitrary
            return

        # Apply
        self.processHelp(name, True)

    @staticmethod
    def restoreHelp():
        return
        """
        hw = pyzo.toolManager.getTool("pyzointeractivehelp")
        if hw:
            hw.restoreCurrent()
        """

    def event(self, event):
        """event(event)

        Overload main event handler so we can pass Ctrl-C Ctr-v etc, to the main
        window.

        """
        if isinstance(event, QtGui.QKeyEvent):
            # Ignore CTRL+{A-Z} since those keys are handled through the menu
            if (
                (event.modifiers() & QtCore.Qt.ControlModifier)
                and (event.key() >= QtCore.Qt.Key_A)
                and (event.key() <= QtCore.Qt.Key_Z)
            ):
                event.ignore()
                return False

        # Default behavior
        CodeEditor.event(self, event)
        return True

    def keyPressEvent(self, event):
        """Receive qt key event.
        From here we'l dispatch the event to perform autocompletion
        or other stuff...
        """

        # Get ordinal key
        ordKey = -1
        if event.text():
            ordKey = ord(event.text()[0])

        # Cancel any introspection in progress
        self._delayTimer._line = ""

        # Invoke autocomplete via tab key?
        if event.key() == QtCore.Qt.Key_Tab and not self.autocompleteActive():
            if self._config.settings.autoComplete:
                cursor = self.textCursor()
                if cursor.position() == cursor.anchor():
                    text = cursor.block().text()[: cursor.positionInBlock()]
                    if text and (text[-1] in (Tokens.ALPHANUM + "._")):
                        self.introspect(True, False)
                        return

        super().keyPressEvent(event)

        # Analyse character/key to determine what introspection to fire
        if ordKey:
            if (
                ordKey >= 48 or ordKey in [8, 46]
            ) and self._config.settings.autoComplete == 1:
                # If a char that allows completion or backspace or dot was pressed
                self.introspect(True)
            elif ordKey >= 32:
                # Printable chars, only calltip
                self.introspect()
        elif event.key() in [QtCore.Qt.Key_Left, QtCore.Qt.Key_Right]:
            self.introspect()


class PyzoEditor(BaseTextCtrl):
    somethingChanged = QtCore.Signal()

    def __init__(self, config, parent=None, **kwds):
        super().__init__(config, parent=parent, showLineNumbers=True, **kwds)

        # Init filename and name
        self._filename = ""
        self._name = "<TMP>"

        self._main = parent
        self._keymapper = KeyMapper(config, self._main)
        self._icons = loadIcons()

        self._parser = Parser()
        self._parser.start()


        # View settings
        # TODO: self.setViewWrapSymbols(view.showWrapSymbols)
        self.setShowLineEndings(self._config.view.showLineEndings)
        self.setShowIndentationGuides(self._config.view.showIndentationGuides)
        #
        self.setWrap(bool(self._config.view.wrap))
        self.setHighlightCurrentLine(self._config.view.highlightCurrentLine)
        self.setLongLineIndicatorPosition(self._config.view.edgeColumn)
        # TODO: self.setFolding( int(view.codeFolding)*5 )
        # bracematch is set in baseTextCtrl, since it also applies to shells
        # dito for zoom and tabWidth

        # Set line endings to default
        self.lineEndings = self._config.settings.defaultLineEndings

        # Set encoding to default
        self.encoding = "UTF-8"

        # Modification time to test file change
        self._modifyTime = 0

        self.modificationChanged.connect(self._onModificationChanged)

        # To see whether the doc has changed to update the parser.
        self.textChanged.connect(self._onModified)

        # This timer is used to hide the marker that shows which code is executed
        self._showRunCursorTimer = QtCore.QTimer()

        # Add context menu (the offset is to prevent accidental auto-clicking)
        self._menu = EditorContextMenu(self, self._main)
        self.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(
            lambda p: self._menu.popup(self.mapToGlobal(p) + QtCore.QPoint(0, 3))
        )

        # Update status bar
        self.cursorPositionChanged.connect(self._updateStatusBar)

    ## Properties

    @property
    def name(self):
        return self._name

    @property
    def filename(self):
        return self._filename

    @property
    def lineEndings(self):
        """
        Line-endings style of this file. Setter accepts machine-readable (e.g. '\r') and human-readable (e.g. 'CR') input
        """
        return self._lineEndings

    @lineEndings.setter
    def lineEndings(self, value):
        if value in ("\r", "\n", "\r\n"):
            self._lineEndings = value
            return
        try:
            self._lineEndings = {"CR": "\r", "LF": "\n", "CRLF": "\r\n"}[value]
        except KeyError:
            raise ValueError("Invalid line endings style %r" % value)

    @property
    def lineEndingsHumanReadable(self):
        """
        Current line-endings style, human readable (e.g. 'CR')
        """
        return {"\r": "CR", "\n": "LF", "\r\n": "CRLF"}[self.lineEndings]

    @property
    def encoding(self):
        """Encoding used to convert the text of this file to bytes."""
        return self._encoding

    @encoding.setter
    def encoding(self, value):
        # Test given value, correct name if it exists
        try:
            c = codecs.lookup(value)
            value = c.name
        except Exception:
            value = codecs.lookup("UTF-8").name
        # Store
        self._encoding = value

    ##

    def justifyText(self):
        """Overloaded version of justifyText to make it use our
        configurable justificationwidth.
        """
        super().justifyText(self._config.settings.justificationWidth)

    def showRunCursor(self, cursor):
        """
        Momentarily highlight a piece of code to show that this is being executed
        """

        extraSelection = QtWidgets.QTextEdit.ExtraSelection()
        extraSelection.cursor = cursor
        extraSelection.format.setBackground(QtCore.Qt.gray)
        self.setExtraSelections([extraSelection])

        self._showRunCursorTimer.singleShot(200, lambda: self.setExtraSelections([]))

    def id(self):
        """Get an id of this editor. This is the filename,
        or for tmp files, the name."""
        if self._filename:
            return self._filename
        else:
            return self._name

    def focusInEvent(self, event):
        """Test whether the file has been changed 'behind our back'"""
        self.testWhetherFileWasChanged()
        return super().focusInEvent(event)

    def testWhetherFileWasChanged(self):
        """testWhetherFileWasChanged()
        Test to see whether the file was changed outside our backs,
        and let the user decide what to do.
        Returns True if it was changed.
        """

        # get the path
        path = self._filename
        if not os.path.isfile(path):
            # file is deleted from the outside
            return

        # test the modification time...
        mtime = os.path.getmtime(path)
        if mtime != self._modifyTime:
            # ask user
            dlg = QtWidgets.QMessageBox(self)
            dlg.setWindowTitle("File was changed")
            dlg.setText(
                "File has been modified outside of the editor:\n" + self._filename
            )
            dlg.setInformativeText("Do you want to reload?")
            t = dlg.addButton("Reload", QtWidgets.QMessageBox.AcceptRole)  # 0
            dlg.addButton("Keep this version", QtWidgets.QMessageBox.RejectRole)  # 1
            dlg.setDefaultButton(t)

            # whatever the result, we will reset the modified time
            self._modifyTime = os.path.getmtime(path)

            # get result and act
            result = dlg.exec_()
            if result == 0:  # in PySide6 AcceptRole != 0
                self.reload()
            else:
                pass  # when cancelled or explicitly said, do nothing

            # Return that indeed the file was changes
            return True

    def _onModificationChanged(self, changed):
        """Handler for the modificationChanged signal. Emit somethingChanged
        for the editorStack to update the modification notice."""
        self.somethingChanged.emit()

    def _onModified(self):
        self._parser.parseThis(self)

    def _updateStatusBar(self):
        return
        """
        editor = pyzo.editors.getCurrentEditor()
        sb = self.parent().statusBar()
        sb.updateCursorInfo(editor)
        sb.updateFileEncodingInfo(editor)
        """

    def dragMoveEvent(self, event):
        """Otherwise cursor can get stuck.
        https://bitbucket.org/iep-project/iep/issue/252
        https://qt-project.org/forums/viewthread/3180
        """
        if event.mimeData().hasUrls():
            event.acceptProposedAction()
        else:
            BaseTextCtrl.dropEvent(self, event)

    def dropEvent(self, event):
        """Drop files in the list."""
        BaseTextCtrl.dropEvent(self, event)
        """
        if event.mimeData().hasUrls():
            # file: let the editorstack do the work.
            pyzo.editors.dropEvent(event)
        else:
            # text: act normal
            BaseTextCtrl.dropEvent(self, event)
        """

    def showEvent(self, event=None):
        """Capture show event to change title."""
        # Act normally
        if event:
            BaseTextCtrl.showEvent(self, event)

        # Make parser update
        self._parser.parseThis(self)

    def setTitleInMainWindow(self):
        """set the title  text in the main window to show filename."""

        # compose title
        if self._main:
            name, path = self._name, self._filename
            if path:
                self._main.setMainTitle(path)
            else:
                self._main.setMainTitle(name)

    def save(self, filename=None):
        """Save the file. No checking is done."""

        # get filename
        if filename is None:
            filename = self._filename
        if not filename:
            raise ValueError("No filename specified, and no filename known.")

        # Test whether it was changed without us knowing. If so, dont save now.
        if self.testWhetherFileWasChanged():
            return

        # Remove whitespace in a single undo-able action
        if (
            self.removeTrailingWS
            or self._config.settings.removeTrailingWhitespaceWhenSaving
        ):
            # Original cursor to put state back at the end
            oricursor = self.textCursor()
            # Screen cursor to select document
            screenCursor = self.textCursor()
            screenCursor.movePosition(screenCursor.Start)
            screenCursor.movePosition(screenCursor.End, screenCursor.KeepAnchor)
            # Cursor for doing the editor
            editCursor = self.textCursor()
            # Go!
            editCursor.beginEditBlock()
            try:
                editCursor.setPosition(screenCursor.selectionStart())
                editCursor.movePosition(editCursor.StartOfBlock)
                while (
                    editCursor.position() < screenCursor.selectionEnd()
                    or editCursor.position() <= screenCursor.selectionStart()
                ):
                    editCursor.movePosition(editCursor.StartOfBlock)
                    editCursor.movePosition(
                        editCursor.EndOfBlock, editCursor.KeepAnchor
                    )
                    text1 = editCursor.selectedText()
                    text2 = text1.rstrip()
                    if len(text1) != len(text2):
                        editCursor.insertText(text2)
                    if not editCursor.block().next().isValid():
                        break
                    editCursor.movePosition(editCursor.NextBlock)
            finally:
                self.setTextCursor(oricursor)
                editCursor.endEditBlock()

        # Get text and convert line endings
        text = self.toPlainText()
        text = text.replace("\n", self.lineEndings)

        # Make bytes
        bb = text.encode(self.encoding)

        # Store
        f = open(filename, "wb")
        try:
            f.write(bb)
        finally:
            f.close()

        # Update stats
        self._filename = normalizePath(filename)
        self._name = os.path.split(self._filename)[1]
        self.document().setModified(False)
        self._modifyTime = os.path.getmtime(self._filename)

        # update title (in case of a rename)
        self.setTitleInMainWindow()

        # allow item to update its texts (no need: onModifiedChanged does this)
        # self.somethingChanged.emit()

    def reload(self):
        """Reload text using the self._filename.
        We do not have a load method; we first try to load the file
        and only when we succeed create an editor to show it in...
        This method is only for reloading in case the file was changed
        outside of the editor."""

        # We can only load if the filename is known
        if not self._filename:
            return
        filename = self._filename

        # Remember where we are
        cursor = self.textCursor()
        linenr = cursor.blockNumber() + 1

        # Load file (as bytes)
        with open(filename, "rb") as f:
            bb = f.read()

        # Convert to text
        text = bb.decode("UTF-8")

        # Process line endings (before setting the text)
        self.lineEndings = determineLineEnding(text)

        # Set text
        self.setPlainText(text)
        self.document().setModified(False)

        # Go where we were (approximately)
        self.gotoLine(linenr)

    def deleteLines(self):
        cursor = self.textCursor()
        # Find start and end of selection
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        # Expand selection: from start of first block to start of next block
        cursor.setPosition(start)
        cursor.movePosition(cursor.StartOfBlock)
        cursor.setPosition(end, cursor.KeepAnchor)
        cursor.movePosition(cursor.NextBlock, cursor.KeepAnchor)

        cursor.removeSelectedText()

    def duplicateLines(self):
        cursor = self.textCursor()
        # Find start and end of selection
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        # Expand selection: from start of first block to start of next block
        cursor.setPosition(start)
        cursor.movePosition(cursor.StartOfBlock)
        cursor.setPosition(end, cursor.KeepAnchor)
        cursor.movePosition(cursor.NextBlock, cursor.KeepAnchor)

        text = cursor.selectedText()
        cursor.setPosition(start)
        cursor.movePosition(cursor.StartOfBlock)
        cursor.insertText(text)

    def commentCode(self):
        """
        Comment the lines that are currently selected
        """
        indents = []
        indentChar = " " if self.indentUsingSpaces() else "\t"

        def getIndent(cursor):
            text = cursor.block().text().rstrip()
            if text:
                indents.append(len(text) - len(text.lstrip()))

        def commentBlock(cursor):
            blockText = cursor.block().text()
            numMissingIndentChars = minindent - (
                len(blockText) - len(blockText.lstrip(indentChar))
            )
            if numMissingIndentChars > 0:
                # Prevent setPosition from leaving bounds of the current block
                # if there are too few indent characters (e.g. an empty line)
                cursor.insertText(indentChar * numMissingIndentChars)
            cursor.setPosition(cursor.block().position() + minindent)
            cursor.insertText("# ")

        self.doForSelectedBlocks(getIndent)
        minindent = min(indents) if indents else 0
        self.doForSelectedBlocks(commentBlock)

    def uncommentCode(self):
        """
        Uncomment the lines that are currently selected
        """
        # TODO: this should not be applied to lines that are part of a multi-line string

        # Define the uncomment function to be applied to all blocks
        def uncommentBlock(cursor):
            """
            Find the first # on the line; if there is just whitespace before it,
            remove the # and if it is followed by a space remove the space, too
            """
            text = cursor.block().text()
            commentStart = text.find("#")
            if commentStart == -1:
                return  # No comment on this line
            if text[:commentStart].strip() != "":
                return  # Text before the #
            # Move the cursor to the beginning of the comment
            cursor.setPosition(cursor.block().position() + commentStart)
            cursor.deleteChar()
            if text[commentStart:].startswith("# "):
                cursor.deleteChar()

        # Apply this function to all blocks
        self.doForSelectedBlocks(uncommentBlock)

    def toggleCommentCode(self):
        def toggleComment():
            """
            Toggles comments for the seclected text in editor, most of the code is
            taken from commentCode and uncommentCode
            """
            text_block = []

            def getBlocks(cursor):
                text = cursor.block().text()
                text_block.append(text)

            def commentBlock(cursor):
                cursor.setPosition(cursor.block().position())
                cursor.insertText("# ")

            def uncommentBlock(cursor):
                """
                Find the first # on the line; if there is just whitespace before it,
                remove the # and if it is followed by a space remove the space, too
                """
                cursor.setPosition(cursor.block().position())
                cursor.deleteChar()
                cursor.deleteChar()

            self.doForSelectedBlocks(getBlocks)
            commented = [item for item in text_block if item.startswith("# ")]
            if len(commented) == len(text_block):
                self.doForSelectedBlocks(uncommentBlock)
            else:
                self.doForSelectedBlocks(commentBlock)

        toggleComment()

    def gotoDef(self):
        """
        Goto the definition for the word under the cursor
        """
        return
        """
        # Get name of object to go to
        cursor = self.textCursor()
        if not cursor.hasSelection():
            cursor.select(cursor.WordUnderCursor)
        word = cursor.selection().toPlainText()

        # Send the open command to the shell
        s = pyzo.shells.getCurrentShell()
        if s is not None:
            if word and word.isidentifier():
                s.executeCommand("open %s\n" % word)
            else:
                s.write("Invalid identifier %r\n" % word)
        """

    ## Introspection processing methods

    def processCallTip(self, cto):
        """Processes a calltip request using a CallTipObject instance."""
        # Try using buffer first
        if cto.tryUsingBuffer():
            return

        # Try obtaining calltip from the source
        sig = self._parser.getFictiveSignature(cto.name, self, True)
        if sig:
            # Done
            cto.finish(sig)
        else:
            return
            """
            # Try the shell
            shell = pyzo.shells.getCurrentShell()
            if shell:
                shell.processCallTip(cto)
            """

    def processAutoComp(self, aco):
        """Processes an autocomp request using an AutoCompObject instance."""

        # Try using buffer first
        if aco.tryUsingBuffer():
            return

        # Init name to poll by remote process (can be changed!)
        # nameForShell = aco.name

        # Get normal fictive namespace
        fictiveNS = self._parser.getFictiveNameSpace(self)
        fictiveNS = set(fictiveNS)

        # Add names
        if not aco.name:
            # "root" names
            aco.addNames(fictiveNS)
            # imports
            importNames, importLines = self._parser.getFictiveImports(self)
            aco.addNames(importNames)
        else:
            # Prepare list of class names to check out
            classNames = [aco.name]
            handleSelf = True
            # Unroll supers
            while classNames:
                className = classNames.pop(0)
                if not className:
                    continue
                if handleSelf or (className in fictiveNS):
                    # Only the self list (only first iter)
                    fictiveClass = self._parser.getFictiveClass(
                        className, self, handleSelf
                    )
                    handleSelf = False
                    if fictiveClass:
                        aco.addNames(fictiveClass.members)
                        classNames.extend(fictiveClass.supers)
                else:
                    # nameForShell = className
                    break

        # If there's a shell, let it finish the autocompletion
        """
        shell = pyzo.shells.getCurrentShell()
        if shell:
            aco.name = nameForShell  # might be the same or a base class
            shell.processAutoComp(aco)
        else:
            # Otherwise we finish it ourselves
            aco.finish()
        """
        aco.finish()


def _test():
    from Qt.QtWidgets import QApplication, QMainWindow

    CONFIG = DotProxy(
        json.load(
            open(r"C:\Users\tyler\src\GitHub\InProgress\pyzo\defaultConfig.json", "r")
        )
    )
    app = QApplication(sys.argv)
    MAIN = QMainWindow()
    ed = PyzoEditor(CONFIG, parent=MAIN)
    MAIN.setCentralWidget(ed)
    MAIN.show()
    sys.exit(app.exec_())


if __name__ == "__main__":
    _test()
