/* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1/GPL 3.0
*
* The contents of this file are subject to the Mozilla Public License Version
* 1.1 (the "License"); you may not use this file except in compliance with
* the License. You may obtain a copy of the License at
* http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Original Code is the Team15 library.
*
* The Initial Developer of the Original Code is
* Vladimir Panteleev <vladimir@thecybershadow.net>
* Portions created by the Initial Developer are Copyright (C) 2007-2011
* the Initial Developer. All Rights Reserved.
*
* Contributor(s):
*
* Alternatively, the contents of this file may be used under the terms of the
* GNU General Public License Version 3 (the "GPL") or later, in which case
* the provisions of the GPL are applicable instead of those above. If you
* wish to allow use of your version of this file only under the terms of the
* GPL, and not to allow others to use your version of this file under the
* terms of the MPL, indicate your decision by deleting the provisions above
* and replace them with the notice and other provisions required by the GPL.
* If you do not delete the provisions above, a recipient may use your version
* of this file under the terms of either the MPL or the GPL.
*
* ***** END LICENSE BLOCK ***** */
/// Provides helper functions to save images.
/// $(Red License Note)
/// This module is licensed under the MPL license due to the savePng method.
module ShardTools.ImageSaver;

import ShardTools.Color;
//import std.hash.crc32;
import crc32;
import core.atomic;
import core.bitop;
import std.zlib;
import ShardTools.PathTools;
import std.file;

// This function is taken from CyberShadow's ae library, from the Image class.
// The Image class is licensed MPL, thus this file is as well.		
/// Saves an array of colours to a file with the specified width and height.
/// Note that this function has a different license than most of this library.
/// License:
/// 	This method was taken from Vladimir Panteleev's ae library and slightly changed to use ShardTools.Color.
/// 	All credits go to Vladimir Panteleev (CyberShadow).
/// 	The license for this function is MPL.
static void savePNG(string filename, Color[] pixels, int w, int h) {	
	string Dir = PathTools.GetDirectoryPath(filename);	
	if(!exists(Dir))
		mkdirRecurse(Dir);
	enum : ulong { SIGNATURE = 0x0a1a0a0d474e5089 }	
	struct PNGChunk {
		char[4] type;
		const(void)[] data;
		uint crc32() {
			//uint crc = rangeToCRC32(cast(ubyte[])type[]);
			uint crc = strcrc32(type[]);
			foreach (ubyte v; cast(ubyte[])data) {
				//crc = updateCRC32(crc, v);
				crc = update_crc32(v, crc);
			}
			return ~crc;
		}

		this(string type, const(void)[] data) {
			this.type[] = type[];
			this.data = data;
		}
	}

	enum PNGColourType : ubyte { G, RGB=2, PLTE, GA, RGBA=6 }
	enum PNGCompressionMethod : ubyte { DEFLATE }
	enum PNGFilterMethod : ubyte { ADAPTIVE }
	enum PNGInterlaceMethod : ubyte { NONE, ADAM7 }

	enum PNGFilterAdaptive : ubyte { NONE, SUB, UP, AVERAGE, PAETH }

	struct PNGHeader {
		align(1):
			uint width, height;
			ubyte colourDepth;
			PNGColourType colourType;
			PNGCompressionMethod compressionMethod;
			PNGFilterMethod filterMethod;
			PNGInterlaceMethod interlaceMethod;
			//static assert(PNGHeader.sizeof == 13);
	}

	alias typeof(Color.init.tupleof[0]) CHANNEL_TYPE;

	enum COLOUR_TYPE = PNGColourType.RGBA;

	PNGChunk[] chunks;
	PNGHeader header = {
		width : bswap(w),
		height : bswap(h),
		colourDepth : CHANNEL_TYPE.sizeof * 8,
		colourType : COLOUR_TYPE,
		compressionMethod : PNGCompressionMethod.DEFLATE,
		filterMethod : PNGFilterMethod.ADAPTIVE,
		interlaceMethod : PNGInterlaceMethod.NONE,
	};
	chunks ~= PNGChunk("IHDR", cast(void[])[header]);

	uint idatStride = cast(uint)(w*Color.sizeof+1);
	ubyte[] idatData = new ubyte[h*idatStride];
	for (uint y=0; y<h; y++) {
		idatData[y*idatStride] = PNGFilterAdaptive.NONE;
		auto rowPixels = cast(Color[])idatData[y*idatStride+1..(y+1)*idatStride];
		rowPixels[] = pixels[y*w..(y+1)*w];
	
		//foreach (ref p; cast(int[])rowPixels)
			//p = bswap(p);
	}
	chunks ~= PNGChunk("IDAT", compress(idatData, 5));
	chunks ~= PNGChunk("IEND", null);

	uint totalSize = 8;
	foreach (chunk; chunks)
		totalSize += 8 + chunk.data.length + 4;
	ubyte[] data = new ubyte[totalSize];

	*cast(ulong*)data.ptr = SIGNATURE;
	uint pos = 8;
	foreach(chunk;chunks) {
		uint i = pos;
		uint chunkLength = cast(uint)chunk.data.length;
		pos += 12 + chunkLength;
		*cast(uint*)&data[i] = bswap(chunkLength);
		(cast(char[])data[i+4 .. i+8])[] = chunk.type[];
		data[i+8 .. i+8+chunk.data.length] = (cast(ubyte[])chunk.data)[];
		*cast(uint*)&data[i+8+chunk.data.length] = bswap(chunk.crc32());
		assert(pos == i+12+chunk.data.length);
	}
	std.file.write(filename, data);
}