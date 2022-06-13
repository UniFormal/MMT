let oldmathjax = MathJax;
MathJax = {
    startup: {
        ready() {
            MathJax.startup.defaultReady();
            const MML = MathJax.startup.document.inputJax[0];
            const adaptor = MML.adaptor;
            MML.mmlFilters.add(function ({math, document, data}) {
                for (const mtext of data.querySelectorAll('mtext')) {
                    const child = mtext.firstElementChild;
                    if (child && child.namespaceURI === 'http://www.w3.org/1999/xhtml') {
                        const semantics = adaptor.node('semantics', {}, [
                            adaptor.node('annotation-xml', {encoding: 'application/xhtml+xml'}, mtext.childNodes)
                        ]);
                        mtext.parentNode.replaceChild(semantics, mtext);
                    }
                }
            });
        }
    }
}