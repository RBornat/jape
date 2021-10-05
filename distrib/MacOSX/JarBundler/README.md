# JarBundler
JarBundler is a feature-rich Ant task which will create a Mac OS X application bundle from a list of Jar files and a main class name. You can add an Icon resource, set various Mac OS X native look-and-feel bells and whistles, and maintain your application bundles as part of your normal build and release cycle.

Project moved from http://sourceforge.net/projects/jarbundler/ to github by ultramixer (tofi86)

This version cloned by Richard Bornat and incorporated into the RBornat/jape github repository (since tofi86 seems to have left it alone) and tidied up a little

# Releases

## Maven
Add the following dependency to your `pom.xml` file:
```xml
<dependency>
    <groupId>com.ultramixer.jarbundler</groupId>
    <artifactId>jarbundler-core</artifactId>
    <version>3.3.0</version>
</dependency>
```

## Download
Check the [release page](https://github.com/UltraMixer/JarBundler/releases) to get the latest distribution.


# Documentation
Take a look at [./docs/index.html](http://htmlpreview.github.io/?https://github.com/UltraMixer/JarBundler/blob/master/dox/index.html) *(Currently outdated. Sorry.)*


# ChangeLog

## Version 3.3.1-forjape (2020-11 or so)
* got rid of deprecations in JarBundler
* JVMVersion also in Oracle plist
* parsing later version numbers (e.g. 9+) in setJVMVersion

## Version 3.3.0 (2015-11-09)
* Merged changes from [tofi86/Jarbundler](https://github.com/tofi86/Jarbundler/) into official release
  * optional `contentSize` attribute *(for Plist key `NSPreferencesContentSize`)*
  * optional `useJavaXKey` attribute *(for [universalJavaApplicationStub](https://github.com/tofi86/universalJavaApplicationStub) support)*
  * optional `allowmixedlocalizations` attribute *(for Plist key `CFBundleAllowMixedLocalizations`)*
  * optional `copyright` attribute *(for Plist key `NSHumanReadableCopyright`)*
  * removed deprecated `aboutmenuname` attribute *(use `shortname` attribute instead)*
  * removed deprecated `infostring` and `shortinfostring` attributes *(use `copyright` attribute instead)*

## Version 3.2.0 (2015-08-05)
* optional `highResolutionCapable` attribute (for Plist key `NSHighResolutionCapable`)
* optional `LSApplicationCategoryType` attribute (for Plist key `LSApplicationCategoryType`)
* optional `SUPublicDSAKeyFile` attribute (for Plist key `SUPublicDSAKeyFile`)

## Previous history
Take a look at [./docs/index.html](http://htmlpreview.github.io/?https://github.com/UltraMixer/JarBundler/blob/master/dox/index.html)


# License
Licensed under `Apache License v2.0`