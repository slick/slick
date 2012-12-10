"""
    Based on sphinx.ext.extlinks,
    :copyright: Copyright 2007-2011 by the Sphinx team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from docutils import nodes, utils

from sphinx.util.nodes import split_explicit_title


def make_link_role(base_url):
    def role(typ, rawtext, text, lineno, inliner, options={}, content=[]):
        text = utils.unescape(text)
        has_explicit_title, title, part = split_explicit_title(text)
        try:
            full_url = base_url % part
        except (TypeError, ValueError):
            inliner.reporter.warning(
                'unable to expand %s apilink with base URL %r, please make '
                'sure the base contains \'%%s\' exactly once'
                % (typ, base_url), line=lineno)
            full_url = base_url + part
        if not has_explicit_title:
            idents = part.split(".")
            title = idents[len(idents)-1]
        pnode = nodes.reference(title, title, internal=False, refuri=full_url)
        return [pnode], []
    return role

def setup_link_roles(app):
    for name, base_url in app.config.apilinks.iteritems():
        app.add_role(name, make_link_role(base_url))

def setup(app):
    app.add_config_value('apilinks', {}, 'env')
    app.connect('builder-inited', setup_link_roles)
