<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" omit-xml-declaration="yes" indent="no"/>

<xsl:template match="/">

      <!-- start Data Section table -->
      <table cellspacing="0" cellpadding="0" class="pagetable">

        <tr>

<xsl:for-each select="pages/page">
          <td>
            <xsl:variable name="pagenumber"><xsl:value-of select="." /></xsl:variable>
            <a class="pageSection" href="javascript: getTableData({$pagenumber});"><xsl:value-of select="$pagenumber" />
            </a>
          </td>
</xsl:for-each>

        </tr>
      </table>

</xsl:template>
</xsl:stylesheet>
