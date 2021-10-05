<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="3.0">
    <xsl:output method="html" encoding="UTF-8"/>
    <xsl:template match="/">
        <xsl:apply-templates select="/array/array"/>
    </xsl:template>

    <!-- page for each comment array -->

    <xsl:template match="/array/array">
        <xsl:result-document encoding="UTF-8" method="xhtml" href="issue-pages/issue-{number}.html">
            <html>
                <head>
                    <title>
                        <xsl:apply-templates select="title"/>
                    </title>
                </head>
                <body>
                    <h1>
                        <xsl:value-of select="number"/>
                        <xsl:text>:&#160;</xsl:text>
                        <xsl:value-of select="title"/>
                    </h1>
                    <div>
                        <p><xsl:apply-templates select="body"></xsl:apply-templates></p>
                    </div>
                </body>
            </html>
        </xsl:result-document>
    </xsl:template>
</xsl:stylesheet>
