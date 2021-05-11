/*
 * Copyright (c) 2021, Richard Bornat <richard@bornat.me.uk>.
 *
 * Derived from ultramixer's jarbundler (see copyright below), and
 * licensed under the Apache 2.0 licence, described in the copyright below.
 */
 
/*
 * Copyright (c) 2015, UltraMixer Digital Audio Solutions <info@ultramixer.com>, Seth J. Morabito <sethm@loomcom.com>
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.rbornat.jrebundler;

import java.util.ArrayList;
import java.util.List;

import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileSet;

import java.lang.String;



public class HelpBook extends MatchingTask {

	private String folderName = null;
	private String name = null;
	private String locale = null;

	private final List<FileList> fileLists = new ArrayList<FileList>();
	private final List<FileSet> fileSets = new ArrayList<FileSet>();


	// Help Book name
	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}


	// Help Book folder name
	public void setFolderName(String folderName) {
		this.folderName = folderName;
	}

	public String getFolderName() {
		return folderName;
	}


	// Help Book locale
	public void setLocale(String locale) {
		this.locale = locale;
	}

	public String getLocale() {
		return locale;
	}

	// Help Book files as a ANT FileList	
	public void addFileList(FileList fileList) {
		fileLists.add(fileList);
	}

	public List<FileList> getFileLists() {
		return fileLists;
	}

	// Help Book files as a ANT FileSet	
	public void addFileSet(FileSet fileSet) {
		fileSets.add(fileSet);
	}

	public List<FileSet> getFileSets() {
		return fileSets;
	}

}
