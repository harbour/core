<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" omit-xml-declaration="yes" indent="no"/>

<xsl:variable name="numCols" select="count(table/header/cell)" />
<xsl:variable name="numToPad" select="23 - count(table/row)" />
<xsl:template match="/">

      <!-- start Data Section table -->
      <table cellspacing="0" cellpadding="0" class="datatable">

        <tr>

<xsl:for-each select="table/header/cell">
          <th><xsl:value-of select="." /></th>
</xsl:for-each>

        </tr>

<xsl:if test="count(table/row) = 0">
        <tr class="blank">
          <td width="100%" colspan="{$numCols}" align="center" style="font-style: italic; padding:10px;">No Records Found</td>
        </tr>
</xsl:if>

<xsl:for-each select="table/row">
  <xsl:variable name="rowClass">
        <xsl:choose>
          <xsl:when test="position() mod 2">even</xsl:when>
          <xsl:otherwise>odd</xsl:otherwise>
        </xsl:choose>
  </xsl:variable>
        <tr class="{$rowClass}">
  <xsl:call-template name="buildCell">
    <xsl:with-param name="rowNode" select="." />
  </xsl:call-template>
        </tr>
</xsl:for-each>

<xsl:call-template name="padding">
  <xsl:with-param name="max_count" select="$numToPad"/>
  <xsl:with-param name="counter" select="'0'"/>
</xsl:call-template>

      </table>
      <!-- end Data Section table -->

</xsl:template>

<xsl:template name="buildCell">
  <xsl:param name="rowNode"/>

  <xsl:for-each select="/table/header/cell">
      <xsl:variable name="colName" select="@key" />
          <td><xsl:value-of select="$rowNode/*[@key=$colName]" disable-output-escaping="yes"/>&#160;</td>
  </xsl:for-each>

</xsl:template>

<xsl:template name="padding">
  <xsl:param name="max_count"/>
  <xsl:param name="counter"/>
  <xsl:if test="$counter &lt; $max_count">
    <tr class="blank">
      <td colspan="{$numCols + 1}">&#160;</td>
    </tr>
    <xsl:call-template name="padding">
      <xsl:with-param name="max_count" select="$max_count"/>
      <xsl:with-param name="counter" select="$counter + 1"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>