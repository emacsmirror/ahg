# commands.py - Mercurial extension to print the list of all defined commands
#
# Copyright (C) 2008 Alberto Griggio
# Author: Alberto Griggio <agriggio@users.sourceforge.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

from mercurial import commands as hgcmd
from mercurial import extensions as hgext
from mercurial import util
from mercurial.i18n import _


def list_commands(ui, repo, **opts):
    """\
Print the list of Mercurial commands.
"""
    short = opts['shortlist']
    debug = not short and opts['debuglist']
    noalias = opts['no_alias']
    noext = short or opts['no_ext']
    cmds = util.set()
    # walk through the main cmdtable
    def process(cmdtable):
        for n in cmdtable:
            aliases = n.split('|')
            if noalias:
                aliases = [aliases[0]]
            for a in aliases:
                if a.startswith('^'):
                    cmds.add(a.lstrip('^'))
                elif a.startswith('debug') and debug:
                    cmds.add(a)
                elif not short:
                    cmds.add(a)
    process(hgcmd.table)
    # and now the cmdtable of each of the loaded extensions
    if not noext:
        for name, ext in hgext.extensions():
            process(ext.cmdtable)
    # ok, now sort the list and print it
    for cmd in util.sort(cmds):
        ui.write(cmd + '\n')

hgcmd.optionalrepo += " commands"

cmdtable = {
    'commands' : (list_commands,
                  [('', 'shortlist', False, _('show only basic commands')),
                   ('', 'debuglist', False, _('show also debugging commands')),
                   ('', 'no-alias', False, _('do not show command aliases')),
                   ('', 'no-ext', False, _('do not show commands defined '
                                           'in extensions'))])
    }
