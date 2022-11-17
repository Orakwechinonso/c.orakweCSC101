<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
    xmlns:xdt="http://www.w3.org/2005/02/xpath-datatypes">
  <xsl:output method ="html"/>

  <!-- ========= -->
  <!-- Templates -->
  <!-- ========= -->

  <!-- T1# - Overall -->
  <xsl:template match="/">
    <style type="text/css">
      h1 {
      font-size: 16.0pt;
      color: #2E74B5;
      font-family: "Calibri Light",sans-serif;
      }

      p.normal {
      margin-top: 0in;
      margin-right: 0in;
      margin-bottom: 8.0pt;
      margin-left: 0in;
      line-height: 105%;
      font-size: 11.0pt;
      font-family: "Calibri",sans-serif;
      }

      h2 {
      margin-top: 2.0pt;
      margin-right: 0in;
      margin-bottom: 0in;
      margin-left: 0in;
      page-break-after: avoid;
      font-size: 13.0pt;
      font-family: "Calibri Light",sans-serif;
      color: #2E74B5;
      line-height: 105%;
      font-weight: normal;
      }

    </style>
    <xsl:call-template name="ShowGeneralInformation" />
    <br />
    <xsl:call-template name="ShowMigrationDetails" />
  </xsl:template>


  <!-- ======= -->
  <!-- Methods -->
  <!-- ======= -->

  <!-- [Method]: to output general information -->
  <xsl:template name="ShowGeneralInformation">
    <h1>We've migrated your customization and settings!</h1>
    <!-- Dispatch to T2.1#, T2.2#, T2.3#, T2.4# -->
    <xsl:apply-templates select="//Message[@Category='SourceProduct']"/>
    <xsl:apply-templates select="//Message[@Category='DestinationProduct']"/>
    <xsl:apply-templates select="//Message[@Category='MigrationTime']"/>
    <xsl:apply-templates select="//Message[@Category='RestoreText']"/>
  </xsl:template>


  <!-- T2.1# - General - SourceProduct -->
  <xsl:template match="//Message[@Category = 'SourceProduct']">
    <p class="normal">
      <xsl:text>From: </xsl:text>
      <b>
        <xsl:copy-of select="* | text()" />
      </b>
    </p>
  </xsl:template>

  <!-- T2.2# - General -DestinationProduct -->
  <xsl:template match="//Message[@Category = 'DestinationProduct']">
    <p class="normal">
      <xsl:text>To:   </xsl:text>
      <b>
        <xsl:copy-of select="* | text()" />
      </b>
    </p>
  </xsl:template>

  <!-- T2.3# - General -MigrationTime -->
  <xsl:template match="//Message[@Category = 'MigrationTime']">
    <p class="normal">
      <xsl:text>Migration Performed on: </xsl:text>
      <b>
        <xsl:copy-of select="* | text()" />
      </b>
    </p>
  </xsl:template>

  <!-- T2.4# - General - RestoreText -->
  <xsl:template match="//Message[@Category = 'RestoreText']">
    <p class="normal">
      <span style='line-height:105%'>
        <xsl:copy-of select="* | text()" />
      </span>
    </p>
  </xsl:template>

  <!-- [Method]: to output Migration Details -->
  <xsl:template name="ShowMigrationDetails">
    <h1>Migration Details</h1>
    <!-- Dispatch to T3#, T4#, T5# -->
    <xsl:apply-templates select="//Section[@Name = 'Profiles']"/>
    <xsl:apply-templates select="//Section[@Name = 'PGP']"/>
    <xsl:apply-templates select="//Section[@Name = 'PlotFiles']"/>
    <xsl:apply-templates select="//Section[@Name = 'DrawingTemplates']"/>
    <xsl:apply-templates select="//Section[@Name = 'HatchPattern']"/>
    <xsl:apply-templates select="//Section[@Name = 'LineType']"/>
    <xsl:apply-templates select="//Section[@Name = 'Shapes']"/>
    <xsl:apply-templates select="//Section[@Name = 'Materials']"/>
  </xsl:template>

  <xsl:variable name="DefaultProfileName" select="//Message[@Category='DefaultProfileName']"/>

  <!-- T3# - Profile details -->
  <xsl:template match="//Section[@Name='Profiles']">
    <h2>
      <b>
        Profiles<br />
      </b>
      <xsl:value-of select="Message[@Category = 'MigrationItemCount']"/>
    </h2>
    <br/>
    <xsl:for-each select="ProfileSection">
      <h2 style="margin-left:.5in">
        <b>
          "<xsl:value-of select="@Name" />"
        </b>
        <xsl:if test="$DefaultProfileName=@Name">
          (current)
        </xsl:if>
      </h2>
      <h2 style="margin-left:.5in">
        Items migrated together with profile:
      </h2>
      <xsl:apply-templates select="Section[@Name = 'ProfileStorage']"/>
      <xsl:apply-templates select="Section[@Name = 'CUI']"/>
      <xsl:apply-templates select="Section[@Name = 'ToolPalettes']"/>
      <xsl:apply-templates select="Section[@Name = 'ContentCatalog']"/>
    </xsl:for-each>
    <br />
  </xsl:template>

  <!-- T3.1# - Profiles - ToolPalettes -->
  <xsl:template match="Section[@Name='ToolPalettes']">
    <h2 style="margin-left:1.0in">
      <b>
        <xsl:value-of select="Message[@Category='Description']" />
      </b>
    </h2>
    <xsl:apply-templates select="SchemeSection"/>
    <br/>
  </xsl:template>

  <!-- T3.1.1# - Profiles - ToolPalettes - Schemes -->
  <xsl:template match="SchemeSection">
    <h2 style="margin-left:1.0in">
      <xsl:value-of select="Message[@Category='MigrationItemCount']"/>
    </h2>
    <xsl:apply-templates select="File[@Category='DetailInfo']"/>
    <xsl:if test="count(Message[@Category='Error']) > 0">
      <h2 style="color:red;margin-left:1.0in">Error</h2>
      <xsl:apply-templates select="Message[@Category='Error']" mode="ProfileItemError" />
      <br/>
    </xsl:if>
  </xsl:template>

  <!-- T3.1.2# - Profiles - ToolPalettes - Schemes - FileMigrations-->
  <xsl:template match="File[@Category='DetailInfo']">
    <p class="normal" style="margin-left:1.0in">
      <xsl:copy-of select="* | text()"/>
    </p>
  </xsl:template>

  <xsl:template match="Section">
    <h2 style="margin-left:.5in">
      <xsl:value-of select="Message[@Category='Description']" />
    </h2>
    <p style="margin-left:.5in">
      <xsl:text> </xsl:text>
      <xsl:copy-of select="Message[@Cagetory='DetailInfo'] | text()" />
    </p>
  </xsl:template>

  <!-- T3.2# - Profiles - CUI -->
  <xsl:template match="Section[@Name='CUI']">
    <h2  style="margin-left:1.0in">
      <b>
        <xsl:value-of select="Message[@Category='Description']" />
      </b>
    </h2>
    <xsl:apply-templates select="File[@Category='MainCUI']"/>
    <xsl:apply-templates select="Message[@Category='MainCUI']"/>
    <xsl:apply-templates select="File[@Category='EnterpriseCUI']"/>
    <xsl:apply-templates select="Message[@Category='EnterpriseCUI']"/>
    <xsl:if test="count(Message[@Category='Error']) > 0">
      <h2 style="color:red;margin-left:1.0in">Error</h2>
      <xsl:apply-templates select="Message[@Category='Error']" mode="ProfileItemError" />
      <br/>
    </xsl:if>
  </xsl:template>

  <!-- T3.2.1# - Profiles - CUI - MainCUI - File -->
  <xsl:template match="File[@Category='MainCUI']">
    <p class="normal" style="margin-left:1.0in">
      Main CUI file set to
      <b>
        <xsl:copy-of select="* | text()"/>
      </b>
    </p>
  </xsl:template>

  <!-- T3.2.2# - Profiles - CUI - EnterpriseCUI - File-->
  <xsl:template match="File[@Category='EnterpriseCUI']">
    <p class="normal" style="margin-left:1.0in">
      Enterprise CUI file set to
      <b>
        <xsl:copy-of select="* | text()"/>
      </b>
    </p>
  </xsl:template>

  <!-- T3.2.3# - Profiles - CUI - MainCUI - Message -->
  <xsl:template match="Message[@Category='MainCUI']">
    <p class="normal" style="margin-left:1.0in">
      <xsl:value-of select="* | text()"/>
    </p>
  </xsl:template>

  <!-- T3.2.4# - Profiles - CUI - EnterpriseCUI - Message-->
  <xsl:template match="Message[@Category='EnterpriseCUI']">
    <p class="normal" style="margin-left:1.0in">
      <xsl:value-of select="* | text()"/>
    </p>
  </xsl:template>

  <!-- T3.3# - Profiles - AWS -->
  <xsl:template match="Section[@Name='ProfileStorage']">
    <h2  style="margin-left:1.0in">
      <b>
        <xsl:value-of select="Message[@Category='Description']" />
      </b>
    </h2>
    <xsl:if test="count(Message[@Category='FromPath'] | Message[@Category='ToPath']) > 0">
      <p class="normal" style="margin-left:1.0in">
        From:
        <i>
          <xsl:value-of select="Message[@Category = 'FromPath']" />
        </i>
        <br />
        To:
        <i>
          <xsl:value-of select="Message[@Category = 'ToPath']" />
        </i>
      </p>
    </xsl:if>
    <xsl:if test="count(Message[@Category='Error']) > 0">
      <h2 style="color:red;margin-left:1.0in">Error</h2>
      <xsl:apply-templates select="Message[@Category='Error']" mode="ProfileItemError" />
      <br/>
    </xsl:if>
  </xsl:template>

  <!-- T3.4# - Profiles - ContentCatalog -->
  <xsl:template match="Section[@Name='ContentCatalog']">
    <h2  style="margin-left:1.0in">
      <b>
        <xsl:value-of select="Message[@Category='Description']" />
      </b>
    </h2>
    <h2 style="margin-left:1.0in">
      <xsl:value-of select="Message[@Category='MigrationItemCount']"/>
    </h2>
    <xsl:apply-templates select="Message[@Category='DetailInfo']"/>
    <xsl:if test="count(Message[@Category='Error']) > 0">
      <h2 style="color:red;margin-left:1.0in">Error</h2>
      <xsl:apply-templates select="Message[@Category='Error']" mode="ProfileItemError" />
    </xsl:if>
    <br/>
  </xsl:template>

  <!-- T3.5# - Profiles - ContentCatalog - Messages -->
  <xsl:template match="Message[@Category='DetailInfo']">
    <p class="normal" style="margin-left:1.0in">
      <xsl:copy-of select="* | text()"/>
    </p>
  </xsl:template>

  <!-- T4# - Categories like PGP, Shapes, Materials, DrawingTemplates-->
  <xsl:template match="Section[@Name='PGP'] | Section[@Name='Shapes'] | Section[@Name='Materials'] | Section[@Name='DrawingTemplates']">
    <h2>
      <b>
        <xsl:value-of select="Message[@Category='Description']" />
        <br />
      </b>
      <xsl:value-of select="Message[@Category='MigrationItemCount']"/>
    </h2>
    <xsl:if test="count(Message[@Category='FromPath'] | Message[@Category='ToPath']) > 0">
      <p class="normal">
        From:
        <i>
          <xsl:value-of select="Message[@Category = 'FromPath']" />
        </i>
        <br />
        To:
        <i>
          <xsl:value-of select="Message[@Category = 'ToPath']" />
        </i>
      </p>
    </xsl:if>
    <xsl:if test="count(Message[@Category='Error']) > 0">
      <h2 style="color:red">Error</h2>
      <xsl:apply-templates select="Message[@Category='Error']"/>
    </xsl:if>
    <br />
  </xsl:template>

  <xsl:template match="Message[@Category='Error']" mode="ProfileItemError">
    <p class="normal" style="margin-bottom: 0in;margin-left:1.0in">
      <xsl:copy-of select="* | text()"/>
    </p>
  </xsl:template>

  <xsl:template match="Message[@Category='Error']">
    <p class="normal" style="margin-bottom: 0in">
      <xsl:copy-of select="* | text()"/>
    </p>
  </xsl:template>

  <!-- T5# - Categories like LineType, HatchPattern, PlotFiles -->
  <xsl:template match="Section[@Name='LineType'] | Section[@Name='HatchPattern']  | Section[@Name='PlotFiles']">
    <h2>
      <b>
        <xsl:value-of select="Message[@Category='Description']" />
      </b>
    </h2>
    <xsl:for-each select="Section[@Name!='Error']">
      <h2>
        <xsl:value-of select="Message[@Category='MigrationItemCount']"/>
      </h2>
      <p class="normal">
        From:
        <i>
          <xsl:value-of select="Message[@Category = 'FromPath']" />
        </i>
        <br />
        To:
        <i>
          <xsl:value-of select="Message[@Category = 'ToPath']" />
        </i>
      </p>
    </xsl:for-each>
    <xsl:apply-templates select="Section[@Name='Error']"/>
    <br />
  </xsl:template>

  <xsl:template match="Section[@Name='Error']">
    <h2 style="color:red">Error</h2>
    <xsl:apply-templates select="Message[@Category='Error']"/>
  </xsl:template>

</xsl:stylesheet>
