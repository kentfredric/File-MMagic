# File::MMagic
#
# $Id: MMagic.pm,v 1.2 1999/06/16 09:25:49 knok Exp $
#
# This program is originated from file.kulp that is a production of The
# Unix Reconstruction Projct.
#    <http://language.perl.com/ppt/index.html>
# Copyright (c) 1999 NOKUBI Takatsugu <knok@daionet.gr.jp>.
#
# There is no warranty for the program.
#
# This product includes software developed by the Apache Group
# for use in the Apache HTTP server project (http://www.apache.org/).
#
# License for the program is followed the original software. The license is
# below.
#
# This program is free and open software. You may use, copy, modify, distribute
# and sell this program (and any modified variants) in any way you wish,
# provided you do not restrict others to do the same, except for the following
# consideration.
#
#I read some of Ian F. Darwin's BSD C implementation, to
#try to determine how some of this was done since the specification
#is a little vague.  I don't believe that this perl version could
#be construed as an "altered version", but I did grab the tokens for
#identifying the hard-coded file types in names.h and copied some of
#the man page.
#
#Here's his notice:
#
#  * Copyright (c) Ian F. Darwin, 1987.
#  * Written by Ian F. Darwin.
#  *
#  * This software is not subject to any license of the American Telephone
#  * and Telegraph Company or of the Regents of the University of California.
#  *
#  * Permission is granted to anyone to use this software for any purpose on
#  * any computer system, and to alter it and redistribute it freely, subject
#  * to the following restrictions:
#  *
#  * 1. The author is not responsible for the consequences of use of this
#  *    software, no matter how awful, even if they arise from flaws in it.
#  *
#  * 2. The origin of this software must not be misrepresented, either by
#  *    explicit claim or by omission.  Since few users ever read sources,
#  *    credits must appear in the documentation.
#  *
#  * 3. Altered versions must be plainly marked as such, and must not be
#  *    misrepresented as being the original software.  Since few users
#  *    ever read sources, credits must appear in the documentation.
#  *
#  * 4. This notice may not be removed or altered.
#
# The following is the Apache License. This program contains the magic file
# that derived from the Apache HTTP Server.
#
#  * Copyright (c) 1995-1999 The Apache Group.  All rights reserved.
#  *
#  * Redistribution and use in source and binary forms, with or without
#  * modification, are permitted provided that the following conditions
#  * are met:
#  *
#  * 1. Redistributions of source code must retain the above copyright
#  *    notice, this list of conditions and the following disclaimer.
#  *
#  * 2. Redistributions in binary form must reproduce the above copyright
#  *    notice, this list of conditions and the following disclaimer in
#  *    the documentation and/or other materials provided with the
#  *    distribution.
#  *
#  * 3. All advertising materials mentioning features or use of this
#  *    software must display the following acknowledgment:
#  *    "This product includes software developed by the Apache Group
#  *    for use in the Apache HTTP server project (http://www.apache.org/)."
#  *
#  * 4. The names "Apache Server" and "Apache Group" must not be used to
#  *    endorse or promote products derived from this software without
#  *    prior written permission. For written permission, please contact
#  *    apache@apache.org.
#  *
#  * 5. Products derived from this software may not be called "Apache"
#  *    nor may "Apache" appear in their names without prior written
#  *    permission of the Apache Group.
#  *
#  * 6. Redistributions of any form whatsoever must retain the following
#  *    acknowledgment:
#  *    "This product includes software developed by the Apache Group
#  *    for use in the Apache HTTP server project (http://www.apache.org/)."
#  *
#  * THIS SOFTWARE IS PROVIDED BY THE APACHE GROUP ``AS IS'' AND ANY
#  * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
#  * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
#  * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#  * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#  * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
#  * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#  * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
#  * OF THE POSSIBILITY OF SUCH DAMAGE.

package File::MMagic;

=head1 NAME

File::MMagic - Guess file type

=head1 SYNOPSIS

  use File::MMagic;
  use FileHandle;

  $mm = new File::MMagic;
  $res = $mm->checktype_filename("/somewhere/unknown/file");

  $fh = new FileHandle "< /somewhere/unknown/file2";
  $res = $mm->checktype_filehandle($fh);

  $fh->read($data, 8192);
  $res = $mm->checktype_contents($data);

=head1 ABSTRACT

This perl library uses perl5 objects to guess file type from filename
and/or filehandle.

=head1 DESCRIPTION

checktype_filename(), checktype_filehandle() and checktype_contents
returns string contains file type with MIME mediatype format.

=head1 COPYRIGHT

This program is originated from file.kulp that is a production of The
Unix Reconstruction Projct.
   <http://language.perl.com/ppt/index.html>
Copyright (c) 1999 NOKUBI Takatsugu <knok@daionet.gr.jp>.

There is no warranty for the program.

This product includes software developed by the Apache Group
for use in the Apache HTTP server project (http://www.apache.org/).

License for the program is followed the original software. The license is
below.

This program is free and open software. You may use, copy, modify, distribute
and sell this program (and any modified variants) in any way you wish,
provided you do not restrict others to do the same, except for the following
consideration.

I read some of Ian F. Darwin's BSD C implementation, to
try to determine how some of this was done since the specification
is a little vague.  I don't believe that this perl version could
be construed as an "altered version", but I did grab the tokens for
identifying the hard-coded file types in names.h and copied some of
the man page.

Here's his notice:

 * Copyright (c) Ian F. Darwin, 1987.
 * Written by Ian F. Darwin.
 *
 * This software is not subject to any license of the American Telephone
 * and Telegraph Company or of the Regents of the University of California.
 *
 * Permission is granted to anyone to use this software for any purpose on
 * any computer system, and to alter it and redistribute it freely, subject
 * to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 *    software, no matter how awful, even if they arise from flaws in it.
 *
 * 2. The origin of this software must not be misrepresented, either by
 *    explicit claim or by omission.  Since few users ever read sources,
 *    credits must appear in the documentation.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.  Since few users
 *    ever read sources, credits must appear in the documentation.
 *
 * 4. This notice may not be removed or altered.

The following is the Apache License. This program contains the magic file
that derived from the Apache HTTP Server.

 * Copyright (c) 1995-1999 The Apache Group.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * 4. The names "Apache Server" and "Apache Group" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * THIS SOFTWARE IS PROVIDED BY THE APACHE GROUP ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.

=cut

use FileHandle;
use strict;

use vars qw(
%TEMPLATES %ESC %SPECIALS $VERSION
$magicFile $checkMagic $followLinks $fileList
$dataLoc
);

BEGIN {
# translation of type in magic file to unpack template and byte count
%TEMPLATES = (byte     => [ 'c', 1 ],
		 ubyte    => [ 'C', 1 ],
		 char     => [ 'c', 1 ],
		 uchar    => [ 'C', 1 ],
		 short    => [ 's', 2 ],
		 ushort   => [ 'S', 2 ],
		 long     => [ 'l', 4 ],
		 ulong    => [ 'L', 4 ],
		 date     => [ 'l', 4 ],
		 ubeshort => [ 'n', 2 ],
		 beshort  => [ [ 'n', 'S', 's' ], 2 ],
		 ubelong  => [   'N',             4 ],
		 belong   => [ [ 'N', 'I', 'i' ], 4 ],
		 bedate   => [   'N',             4 ],
		 uleshort => [   'v',             2 ],
		 leshort  => [ [ 'v', 'S', 's' ], 2 ],
		 ulelong  => [   'V',             4 ],
		 lelong   => [ [ 'V', 'I', 'i' ], 4 ],
		 ledate   => [   'V',             4 ],
		 string   => undef);

# for letter escapes in magic file
%ESC = ( n => "\n",
	    r => "\r",
	    b => "\b",
	    t => "\t",
	    f => "\f",
	    v => "\v" );

# from the BSD names.h, some tokens for hard-coded checks of
# different texts.  This isn't rocket science.  It's prone to
# failure so these checks are only a last resort.
%SPECIALS = 	(
		 "text/rfc822" => [ "Received:",   
			     ">From",       
			     "Return-Path:",
			     "Cc:",         ],
		 "text/news" => [ "Newsgroups:", 
			     "Path:",       
			     "Organization:" ],
		 "text/html" => [ "<html>",
			     "<HTML>",
			     "<head>",
			     "<HEAD>",
			     "<title>",
			     "<TITLE>",
			     "<h1>",
			     "<H1>",
			     "<!--",
			     "<!DOCTYPE",
			],
		 "text/x-roff" => [
			      "\.SH",
			      "\.PP",
			      "\.TH",
			      "\.BR",
			      "\.SS",
			      "\.TP",
			      "\.IR",
				   ],
		);

$VERSION = "0.14";
undef $dataLoc;
}

sub new {
    my $self = {};
    my $proto = shift;
    my $class = ref($proto) || $proto;
    $self->{MF} = [];
    $self->{magic} = [];
    if (! @_) {
	my $fh = *File::MMagic::DATA{IO};
	$dataLoc = $fh->tell() if (! defined $dataLoc);
	$fh->seek($dataLoc, 0);
	&readMagicHandle($self, $fh);
    }
    bless($self);
    return $self;
}

sub readMagicHandle {
    my $self = shift;
    my $fh = shift;
    $self->{MF}->[0] = $fh;
    $self->{MF}->[1] = undef;
    $self->{MF}->[2] = 0;
    readMagicEntry($self->{magic}, $self->{MF});
}

# Not implimented.
#
#sub readMagicFile {
#    my $self = shift;
#    my $mfile = shift;
#}

sub checktype_filename {
    my $self = shift;

# iterate over each file explicitly so we can seek
    my $file = shift;

    # the description line.  append info to this string
    my $desc;
    my $mtype;

    # 0) check permission
    if (! -r $file) {
	$desc .= " can't read `$file': Permission denied.";
	return "x-system/x-error; $desc";
    }

    # 1) check for various special files first
    if ($followLinks) { stat($file); } else { lstat($file); }
    if (! -f _  or -z _) {
	if ( !$followLinks && -l _ ) { 
	    $desc .= " symbolic link to ".readlink($file); 
	}
	elsif ( -d _ ) { $desc .= " directory"; }
	elsif ( -p _ ) { $desc .= " named pipe"; }
	elsif ( -S _ ) { $desc .= " socket"; }
	elsif ( -b _ ) { $desc .= " block special file"; }
	elsif ( -c _ ) { $desc .= " character special file"; }
	elsif ( -z _ ) { $desc .= " empty"; }
	else { $desc .= " special"; }

	return "x-system/x-unix; $desc";
    }

    # current file handle.  or undef if checkMagic (-c option) is true.
    my $fh;

#    $fh = new FileHandle "< $file" or die "$F: $file: $!\n" ;
    $fh = new FileHandle "< $file" or return "x-system/x-error; $file: $!\n" ;

    # 2) check for script
    if (-x $file && -T _) {

	# Note, some magic files include elaborate attempts
	# to match #! header lines and return pretty responses
	# but this slows down matching and is unnecessary.
	my $line1 = <$fh>;
	if ($line1 =~ /^\#!\s*(\S+)/) {
	    $desc .= " executable $1 script text";
	}
	else { $desc .= " commands text"; }

	$fh->close();

	return "x-system/x-unix; $desc";

    }

    return checktype_filehandle($self, $fh, $desc);
}

sub checktype_filehandle {
    my $self = shift;
    my ($fh, $desc) = @_;
    my $mtype;

    # 3) iterate over each magic entry.
    my $matchFound = 0;
    my $m;
    for ($m = 0; $m <= $#{$self->{magic}}; $m++) {

	# check if the m-th magic entry matches
	# if it does, then $desc will contain an updated description
	if (magicMatch($self->{magic}->[$m],\$desc,$fh)) {
	    $matchFound = 1;
	    $mtype = $desc;
	    last;
	}

	# read another entry from the magic file if we've exhausted
	# all the entries already buffered.  readMagicEntry will
	# add to the end of the array if there are more.
	if ($m == $#{$self->{magic}} && !$self->{MF}->[0]->eof()) {
	    readMagicEntry($self->{magic}, $self->{MF});
	}
    }

    # 4) check if it's text or binary.
    # if it's text, then do a bunch of searching for special tokens
    if (!$matchFound) {
	my $data;
	$fh->seek(0,0);
	$fh->read($data, 8192);
	$mtype = checktype_data($self, $data);
    }

    $fh->close();
    $mtype = 'text/plain' if (! defined $mtype);

    return $mtype;
}

sub checktype_contents {
    my $self = shift;
    my $data = shift;
    my $desc;
    my $mtype;

    # 3) iterate over each magic entry.
    my $matchFound = 0;
    my $m;
    for ($m = 0; $m <= $#{$self->{magic}}; $m++) {

	# check if the m-th magic entry matches
	# if it does, then $desc will contain an updated description
	if (magicMatchStr($self->{magic}->[$m],\$desc,$data)) {
	    $matchFound = 1;
	    $mtype = $desc;
	    last;
	}

	# read another entry from the magic file if we've exhausted
	# all the entries already buffered.  readMagicEntry will
	# add to the end of the array if there are more.
	if ($m == $#{$self->{magic}} && !$self->{MF}->[0]->eof()) {
	    readMagicEntry($self->{magic}, $self->{MF});
	}
    }

    # 4) check if it's text or binary.
    # if it's text, then do a bunch of searching for special tokens
    if (!$matchFound) {
	$mtype = checktype_data($self, $data);
    }

    $mtype = 'text/plain' if (! defined $mtype);

    return $mtype;
}

sub checktype_data {
    my $self = shift;
    my $data = shift;
    my $mtype;

    if (check_binary($data)) {
	$mtype = "application/octet-stream";
    } else {
	# in BSD's version, there's an effort to search from
	# more specific to less, but I don't do that.
	my ($type,$token);
	foreach $type (keys %SPECIALS) {
	    foreach $token (@{$SPECIALS{$type}}) {
		# we could do \b word boundaries if the end chars in
		# $token were always \w, but they're not.  this is
		    # crude guessing anyway.
		if ($data =~ /\Q$token\E/m) {
		    $mtype = $type;
		    goto ALLDONE;
		}
	    }
	}
	
      ALLDONE:
	$mtype = 'text/plain' if (! defined $mtype);
    }
	
    $mtype = 'text/plain' if (! defined $mtype);
    return $mtype;
}

sub check_binary {
    my ($data) = @_;
    my $len = length($data);
    my $count = ($data =~ tr/[\x00-\x1f]//);
    return 1 if ($len <= 0); # no contents
    return 1 if (($count/$len) > 0.1); # binary
    return 0;
}

#if ($checkMagic) {
#    # read the whole file if we haven't already
#    while (!$$MF[0]->eof()) {
#	readMagicEntry(\@magic,$MF);
#    }
#    dumpMagic(\@magic);
#}

####### SUBROUTINES ###########

# compare the magic item with the filehandle.
# if success, print info and return true.  otherwise return undef.
#
# this is called recursively if an item has subitems.
sub magicMatch {
    my ($item, $p_desc, $fh) = @_;

    # delayed evaluation.  if this is our first time considering
    # this item, then parse out its structure.  @$item is just the
    # raw string, line number, and subtests until we need the real info.
    # this saves time otherwise wasted parsing unused subtests.
    $item = readMagicLine(@$item) if @$item == 3;

    # $item could be undef if we ran into troubles while reading
    # the entry.
    return unless defined($item);

    # $fh is not be defined if -c.  that way we always return
    # false for every item which allows reading/checking the entire
    # magic file.
    return unless defined($fh);
    
    my ($offtype, $offset, $numbytes, $type, $mask, $op, $testval, 
	$template, $message, $subtests) = @$item;

    # bytes from file
    my $data;

    # set to true if match
    my $match = 0;

    # offset = [ off1, sz, template, off2 ] for indirect offset
    if ($offtype == 1) {
	my ($off1, $sz, $template, $off2) = @$offset;
	$fh->seek($off1,0) or return;
	if ($fh->read($data,$sz) != $sz) { return };
	$off2 += unpack($template,$data);
	$fh->seek($off2,0) or return;
    }
    elsif ($offtype == 2) {
	# relative offsets from previous seek
	$fh->seek($offset,1) or return;
    }
    else {
	# absolute offset
	$fh->seek($offset,0) or return;
    }

    if ($type eq 'string') {
	# read the length of the match string unless the
	# comparison is '>' ($numbytes == 0), in which case 
	# read to the next null or "\n". (that's what BSD's file does)
	if ($numbytes > 0) {
	    if ($fh->read($data,$numbytes) != $numbytes) { return; }
	}
	else {
	    my $ch = $fh->getc();
	    while (defined($ch) && $ch ne "\0" && $ch ne "\n") {
		$data .= $ch;
		$ch = $fh->getc();
	    }
	}

	# now do the comparison
	if ($op eq '=') {
	    $match = ($data eq $testval);
	}
	elsif ($op eq '<') {
	    $match = ($data lt $testval);
	}
	elsif ($op eq '>') {
	    $match = ($data gt $testval);
	}
	# else bogus op, but don't die, just skip

	if ($checkMagic) {
	    print STDERR "STRING: $data $op $testval => $match\n";
	}

    }
    else {
	#numeric

	# read up to 4 bytes
	if ($fh->read($data,$numbytes) != $numbytes) { return; }

	# If template is a ref to an array of 3 letters, 
	# then this is an endian 
	# number which must be first unpacked into an unsigned and then
	# coerced into a signed.  Is there a better way?
	if (ref($template)) {
	    $data = unpack($$template[2],
			   pack($$template[1],
				unpack($$template[0],$data)));
	}
	else {
	    $data = unpack($template,$data);
	}

	# if mask
	if (defined($mask)) {
	    $data &= $mask;
	}

	# Now do the check
	if ($op eq '=') {
	    $match = ($data == $testval);
	}
	elsif ($op eq 'x') {
	    $match = 1;
	}
	elsif ($op eq '!') {
	    $match = ($data != $testval);
	}
	elsif ($op eq '&') {
	    $match = (($data & $testval) == $testval);
	}
	elsif ($op eq '^') {
	    $match = ((~$data & $testval) == $testval);
	}
	elsif ($op eq '<') {
	    $match = ($data < $testval);
	}
	elsif ($op eq '>') {
	    $match = ($data > $testval);
	}
	# else bogus entry that we're ignoring

	if ($checkMagic) {
	    print STDERR "NUMERIC: $data $op $testval => $match\n";
	}

    }

    if ($match) {
	# it's pretty common to find "\b" in the message, but
	# sprintf doesn't insert a backspace.  if it's at the
	# beginning (typical) then don't include separator space.
	if ($message =~ s/^\\b//) {
	    $$p_desc .= sprintf($message,$data);
	}
	else {
#	    $$p_desc .= ' ' . sprintf($message,$data) if $message;
	    $$p_desc .= sprintf($message,$data) if $message;
	}

	my $subtest;
	foreach $subtest (@$subtests) {
	    magicMatch($subtest,$p_desc,$fh);
	}

	return 1;
    }
    
}

sub magicMatchStr {
    my ($item, $p_desc, $str) = @_;

    # delayed evaluation.  if this is our first time considering
    # this item, then parse out its structure.  @$item is just the
    # raw string, line number, and subtests until we need the real info.
    # this saves time otherwise wasted parsing unused subtests.
    $item = readMagicLine(@$item) if @$item == 3;

    # $item could be undef if we ran into troubles while reading
    # the entry.
    return unless defined($item);

    # $fh is not be defined if -c.  that way we always return
    # false for every item which allows reading/checking the entire
    # magic file.
    return unless defined($str);
    
    my ($offtype, $offset, $numbytes, $type, $mask, $op, $testval, 
	$template, $message, $subtests) = @$item;

    # bytes from file
    my $data;

    # set to true if match
    my $match = 0;

    # offset = [ off1, sz, template, off2 ] for indirect offset
    if ($offtype == 1) {
	my ($off1, $sz, $template, $off2) = @$offset;
	return if (length($str) < $off1);
	$data = substr($str, 0, $sz);
	$off2 += unpack($template,$data);
	return if (length($str) < $off2);
    }
    elsif ($offtype == 2) {
	# can't handle relative offsets from previous seek
	return;
    }
    else {
	# absolute offset
	# nothing to do.
    }

    if ($type eq 'string') {
	# read the length of the match string unless the
	# comparison is '>' ($numbytes == 0), in which case 
	# read to the next null or "\n". (that's what BSD's file does)
	if ($numbytes > 0) {
	    $data = substr($str, 0, $numbytes);
	}
	else {
	    $str =~ /^(.*)\0|$/;
	    $data = $1;
	}

	# now do the comparison
	if ($op eq '=') {
	    $match = ($data eq $testval);
	}
	elsif ($op eq '<') {
	    $match = ($data lt $testval);
	}
	elsif ($op eq '>') {
	    $match = ($data gt $testval);
	}
	# else bogus op, but don't die, just skip

	if ($checkMagic) {
	    print STDERR "STRING: $data $op $testval => $match\n";
	}

    }
    else {
	#numeric

	# read up to 4 bytes
	$data = substr($str, 0, 4);

	# If template is a ref to an array of 3 letters, 
	# then this is an endian 
	# number which must be first unpacked into an unsigned and then
	# coerced into a signed.  Is there a better way?
	if (ref($template)) {
	    $data = unpack($$template[2],
			   pack($$template[1],
				unpack($$template[0],$data)));
	}
	else {
	    $data = unpack($template,$data);
	}

	# if mask
	if (defined($mask)) {
	    $data &= $mask;
	}

	# Now do the check
	if ($op eq '=') {
	    $match = ($data == $testval);
	}
	elsif ($op eq 'x') {
	    $match = 1;
	}
	elsif ($op eq '!') {
	    $match = ($data != $testval);
	}
	elsif ($op eq '&') {
	    $match = (($data & $testval) == $testval);
	}
	elsif ($op eq '^') {
	    $match = ((~$data & $testval) == $testval);
	}
	elsif ($op eq '<') {
	    $match = ($data < $testval);
	}
	elsif ($op eq '>') {
	    $match = ($data > $testval);
	}
	# else bogus entry that we're ignoring

	if ($checkMagic) {
	    print STDERR "NUMERIC: $data $op $testval => $match\n";
	}

    }

    if ($match) {
	# it's pretty common to find "\b" in the message, but
	# sprintf doesn't insert a backspace.  if it's at the
	# beginning (typical) then don't include separator space.
	if ($message =~ s/^\\b//) {
	    $$p_desc .= sprintf($message,$data);
	}
	else {
#	    $$p_desc .= ' ' . sprintf($message,$data) if $message;
	    $$p_desc .= sprintf($message,$data) if $message;
	}

	my $subtest;
#	foreach $subtest (@$subtests) {
#	    magicMatch($subtest,$p_desc,$fh);
#	}

	return 1;
    }
    
}

# readMagicEntry($pa_magic, $MF, $depth)
#
# reads the next entry from the magic file and stores it as
# a ref to an array at the end of @$pa_magic.
#
# $MF = [ filehandle, last buffered line, line count ]
#
# This is called recursively with increasing $depth to read in sub-clauses
#
# returns the depth of the current buffered line.
#
sub readMagicEntry {
    my ($pa_magic, $MF, $depth) = @_;

    # for some reason I need a local var because <$$MF[0]> doesn't work.(?)
    my $magicFH = $$MF[0];

    # a ref to an array containing a magic line's components
    my ($entry, $line);

    $line = $$MF[1];		# buffered last line
    while (1) {
	if ($line =~ /^\#/ || $line =~ /^\s*$/) {
	    last if $magicFH->eof();
	    $line = <$magicFH>;
	    $$MF[2]++;
	    next;
	}
	
	my ($thisDepth) = ($line =~ /^(>+)/);

	if (length($thisDepth) > $depth) {
	    $$MF[1] = $line;

	    # call ourselves recursively.  will return the depth
	    # of the entry following the nested group.
	    if (readMagicEntry($entry->[2], $MF, $depth+1) < $depth ||
		$$MF[0]->eof())
	    {
		return;
	    }
	    $line = $$MF[1];
	}
	elsif (length($thisDepth) < $depth) {
	    $$MF[1] = $line;
	    return length($thisDepth);
	}
	elsif (defined(@$entry)) {
	    # already have an entry.  this is not a continuation.
	    # save this line for the next call and exit.
	    $$MF[1] = $line;
	    return length($thisDepth);
	}
	else {
	    # we're here if the number of '>' is the same as the
	    # current depth and we haven't read a magic line yet.

	    # create temp entry
	    # later -- if we ever get around to evaluating this condition --
	    # we'll replace @$entry with the results from readMagicLine.
	    $entry = [ $line , $$MF[2], [] ];

	    # add to list
	    push(@$pa_magic,$entry);

	    # read the next line
	    last if $magicFH->eof();
	    $line = <$magicFH>;
	    $$MF[2]++;
	}
    }
}

# readMagicLine($line, $line_num, $subtests)
#
# parses the match info out of $line.  Returns a reference to an array.
#
#  Format is:
#
# [ offset, bytes, type, mask, operator, testval, template, sprintf, subtests ]
#     0      1      2       3        4         5        6        7      8
#
# subtests is an array like @$pa_magic.
#
sub readMagicLine {
    my ($line, $line_num, $subtests) = @_;

    my ($offtype, $offset, $numbytes, $type, $mask, 
	$operator, $testval, $template, $message);
    
    # this would be easier if escaped whitespace wasn't allowed.

    # grab the offset and type.  offset can either be a decimal, oct,
    # or hex offset or an indirect offset specified in parenthesis
    # like (x[.[bsl]][+-][y]), or a relative offset specified by &.
    # offtype : 0 = absolute, 1 = indirect, 2 = relative
    if ($line =~ s/^>*([&\(]?[a-flsx\.\+\-\d]+\)?)\s+(\S+)\s+//) {
	($offset,$type) = ($1,$2);

	if ($offset =~ /^\(/) {
	    # indirect offset.  
	    $offtype = 1;

	    # store as a reference [ offset1 type template offset2 ]

	    my ($o1,$type,$o2);
	    if (($o1,$type,$o2) = ($offset =~ /\((\d+)(\.[bsl])?([\+\-]?\d+)?\)/))
	    {
		$o1 = oct($o1) if $o1 =~ /^0/o;
		$o2 = oct($o2) if $o2 =~ /^0/o;

		$type =~ s/\.//;
		if ($type eq '') { $type = 'l'; }  # default to long
		$type =~ tr/b/c/; # type will be template for unpack

		my $sz = $type;	  # number of bytes
		$sz =~ tr/csl/124/;

		$offset = [ $o1,$sz,$type,int($o2) ];
	    } else {
		warn "Bad indirect offset at line $line_num. '$offset'\n";
		return;
	    }
	}
	elsif ($offset =~ /^&/o) {
	    # relative offset
	    $offtype = 2;

	    $offset = substr($offset,1);
	    $offset = oct($offset) if $offset =~ /^0/o;
	}
	else {
	    # normal absolute offset
	    $offtype = 0;

	    # convert if needed
	    $offset = oct($offset) if $offset =~ /^0/o;
	}
    }
    else {
	warn "Bad Offset/Type at line $line_num. '$line'\n";
	return;
    }
    
    # check for & operator on type
    if ($type =~ s/&(.*)//) {
	$mask = $1;

	# convert if needed
	$mask = oct($mask) if $mask =~ /^0/o;
    }
    
    # check if type is valid
    if (!exists($TEMPLATES{$type})) {
	warn "Invalid type '$type' at line $line_num\n";
	return;
    }
    
    # take everything after the first non-escaped space
    if ($line =~ s/([^\\])\s+(.*)/$1/) {
	$message = $2;
    }
    else {
	warn "Missing or invalid test condition or message at line $line_num\n";
	return;
    }
    
    # remove the return if it's still there
    $line =~ s/\n$//o;

    # get the operator.  if 'x', must be alone.  default is '='.
    if ($line =~ s/^([><&^=!])//o) {
	$operator = $1;
    }
    elsif ($line eq 'x') {
	$operator = 'x';
    }
    else { $operator = '='; }
    

    if ($type eq 'string') {
	$testval = $line;

	# do octal/hex conversion
	$testval =~ s/\\([x0-7][0-7]?[0-7]?)/chr(oct($1))/eg;

	# do single char escapes
	$testval =~ s/\\(.)/$ESC{$1}||$1/eg;

	# put the number of bytes to read in numbytes.
	# '0' means read to \0 or \n.
	if ($operator =~ /[>x]/o) {
	    $numbytes = 0;
	}
	elsif ($operator =~ /[=<]/o) {
	    $numbytes = length($testval);
	}
	elsif ($operator eq '!') {
	    # annoying special case.  ! operator only applies to numerics so
	    # put it back.
	    $testval = $operator . $testval;
	    $numbytes = length($testval);
	    $operator = '=';
	}
	else {
	    # there's a bug in my magic file where there's
	    # a line that says "0	string	^!<arc..." and the BSD
	    # file program treats the argument like a numeric.  To minimize
	    # hassles, complain about bad ops only if -c is set.
	    warn "Invalid operator '$operator' for type 'string' at line $line_num.\n"
	      if $checkMagic;
	    return;
	}
    }
    else {
	# numeric
	if ($operator ne 'x') {
	    # this conversion is very forgiving.  it's faster and
	    # it doesn't complain about bugs in popular magic files,
	    # but it will silently turn a string into zero.
	    if ($line =~ /^0/o) {
		$testval = oct($line);
	    } else {
		$testval = int($line);
	    }
	}

	($template,$numbytes) = @{$TEMPLATES{$type}};

	# unset coercion of $unsigned unless we're doing order comparison
	if (ref($template)) {
	    $template = $$template[0]
	      unless $operator eq '>' || $operator eq '<';
	}
    }
    
    return [ $offtype, $offset, $numbytes, $type, $mask,
	    $operator, $testval, $template, $message, $subtests ];
}

# recursively write the magic file to stderr.  Numbers are written
# in decimal.
sub dumpMagic {
    my ($magic,$depth) = @_;

    my $entry;
    foreach $entry (@$magic) {
	# delayed evaluation.
	$entry = readMagicLine(@$entry) if @$entry == 3;

	next if !defined($entry);

	my ($offtype, $offset, $numbytes, $type, $mask, $op, $testval, 
	    $template, $message, $subtests) = @$entry;

	print STDERR '>'x$depth;
	if ($offtype == 1) {
	    $offset->[2] =~ tr/c/b/; 
	    print STDERR "($offset->[0].$offset->[2]$offset->[3])";
	}
	elsif ($offtype == 2) {
	    print STDERR "&",$offset;
	}
	else {
	    # offtype == 0
	    print STDERR $offset;
	}
	print STDERR "\t",$type;
	if ($mask) { print STDERR "&",$mask; }
	print STDERR "\t",$op,$testval,"\t",$message,"\n";

	if ($subtests) {
	    dumpMagic($subtests,$depth+1);
	}
    }
}

1;
__DATA__
# Magic data for mod_mime_magic Apache module (originally for file(1) command)
# The module is described in htdocs/manual/mod/mod_mime_magic.html
#
# The format is 4-5 columns:
#    Column #1: byte number to begin checking from, ">" indicates continuation
#    Column #2: type of data to match
#    Column #3: contents of data to match
#    Column #4: MIME type of result
#    Column #5: MIME encoding of result (optional)

#------------------------------------------------------------------------------
# Localstuff:  file(1) magic for locally observed files
# Add any locally observed files here.

#------------------------------------------------------------------------------
# end local stuff
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Java

0	short		0xcafe
>2	short		0xbabe		application/java

#------------------------------------------------------------------------------
# audio:  file(1) magic for sound formats
#
# from Jan Nicolai Langfeldt <janl@ifi.uio.no>,
#

# Sun/NeXT audio data
0	string		.snd
>12	belong		1		audio/basic
>12	belong		2		audio/basic
>12	belong		3		audio/basic
>12	belong		4		audio/basic
>12	belong		5		audio/basic
>12	belong		6		audio/basic
>12	belong		7		audio/basic

>12	belong		23		audio/x-adpcm

# DEC systems (e.g. DECstation 5000) use a variant of the Sun/NeXT format
# that uses little-endian encoding and has a different magic number
# (0x0064732E in little-endian encoding).
0	lelong		0x0064732E	
>12	lelong		1		audio/x-dec-basic
>12	lelong		2		audio/x-dec-basic
>12	lelong		3		audio/x-dec-basic
>12	lelong		4		audio/x-dec-basic
>12	lelong		5		audio/x-dec-basic
>12	lelong		6		audio/x-dec-basic
>12	lelong		7		audio/x-dec-basic
#                                       compressed (G.721 ADPCM)
>12	lelong		23		audio/x-dec-adpcm

# Bytes 0-3 of AIFF, AIFF-C, & 8SVX audio files are "FORM"
#					AIFF audio data
8	string		AIFF		audio/x-aiff	
#					AIFF-C audio data
8	string		AIFC		audio/x-aiff	
#					IFF/8SVX audio data
8	string		8SVX		audio/x-aiff	

# Creative Labs AUDIO stuff
#					Standard MIDI data
0	string	MThd			audio/unknown	
#>9 	byte	>0			(format %d)
#>11	byte	>1			using %d channels
#					Creative Music (CMF) data
0	string	CTMF			audio/unknown	
#					SoundBlaster instrument data
0	string	SBI			audio/unknown	
#					Creative Labs voice data
0	string	Creative\ Voice\ File	audio/unknown	
## is this next line right?  it came this way...
#>19	byte	0x1A
#>23	byte	>0			- version %d
#>22	byte	>0			\b.%d

# [GRR 950115:  is this also Creative Labs?  Guessing that first line
#  should be string instead of unknown-endian long...]
#0	long		0x4e54524b	MultiTrack sound data
#0	string		NTRK		MultiTrack sound data
#>4	long		x		- version %ld

# Microsoft WAVE format (*.wav)
# [GRR 950115:  probably all of the shorts and longs should be leshort/lelong]
#					Microsoft RIFF
0	string		RIFF		audio/unknown	
#					- WAVE format
>8	string		WAVE		audio/x-wav	

#------------------------------------------------------------------------------
# c-lang:  file(1) magic for C programs or various scripts
#

# XPM icons (Greg Roelofs, newt@uchicago.edu)
# ideally should go into "images", but entries below would tag XPM as C source
0	string		/*\ XPM		image/x-xbm

# this first will upset you if you're a PL/1 shop... (are there any left?)
# in which case rm it; ascmagic will catch real C programs
#					C or REXX program text
0	string		/*		text/plain
#					C++ program text
0	string		//		text/plain

#------------------------------------------------------------------------------
# compress:  file(1) magic for pure-compression formats (no archives)
#
# compress, gzip, pack, compact, huf, squeeze, crunch, freeze, yabba, whap, etc.
#
# Formats for various forms of compressed data
# Formats for "compress" proper have been moved into "compress.c",
# because it tries to uncompress it to figure out what's inside.

# standard unix compress
#0	string		\037\235	application/octet-stream	x-compress
0	string		\037\235	application/x-compress

# gzip (GNU zip, not to be confused with [Info-ZIP/PKWARE] zip archiver)
#0       string          \037\213        application/octet-stream	x-gzip
0       string          \037\213        application/x-gzip

# According to gzip.h, this is the correct byte order for packed data.
0	string		\037\036	application/octet-stream
#
# This magic number is byte-order-independent.
#
0	short		017437		application/octet-stream

# XXX - why *two* entries for "compacted data", one of which is
# byte-order independent, and one of which is byte-order dependent?
#
# compacted data
0	short		0x1fff		application/octet-stream
0	string		\377\037	application/octet-stream
# huf output
0	short		0145405		application/octet-stream

# Squeeze and Crunch...
# These numbers were gleaned from the Unix versions of the programs to
# handle these formats.  Note that I can only uncrunch, not crunch, and
# I didn't have a crunched file handy, so the crunch number is untested.
#				Keith Waclena <keith@cerberus.uchicago.edu>
#0	leshort		0x76FF		squeezed data (CP/M, DOS)
#0	leshort		0x76FE		crunched data (CP/M, DOS)

# Freeze
#0	string		\037\237	Frozen file 2.1
#0	string		\037\236	Frozen file 1.0 (or gzip 0.5)

# lzh?
#0	string		\037\240	LZH compressed data

#------------------------------------------------------------------------------
# frame:  file(1) magic for FrameMaker files
#
# This stuff came on a FrameMaker demo tape, most of which is
# copyright, but this file is "published" as witness the following:
#
0	string		\<MakerFile	application/x-frame
0	string		\<MIFFile	application/x-frame
0	string		\<MakerDictionary	application/x-frame
0	string		\<MakerScreenFon	application/x-frame
0	string		\<MML		application/x-frame
0	string		\<Book		application/x-frame
0	string		\<Maker		application/x-frame

#------------------------------------------------------------------------------
# html:  file(1) magic for HTML (HyperText Markup Language) docs
#
# from Daniel Quinlan <quinlan@yggdrasil.com>
#
0	string		\<HEAD		text/html
0	string		\<head		text/html
0	string		\<TITLE		text/html
0	string		\<title		text/html
0       string          \<html          text/html
0       string          \<HTML          text/html
0	string		\<!--		text/html
0	string		\<h1		text/html
0	string		\<H1		text/html

#------------------------------------------------------------------------------
# images:  file(1) magic for image formats (see also "c-lang" for XPM bitmaps)
#
# originally from jef@helios.ee.lbl.gov (Jef Poskanzer),
# additions by janl@ifi.uio.no as well as others. Jan also suggested
# merging several one- and two-line files into here.
#
# XXX - byte order for GIF and TIFF fields?
# [GRR:  TIFF allows both byte orders; GIF is probably little-endian]
#

# [GRR:  what the hell is this doing in here?]
#0	string		xbtoa		btoa'd file

# PBMPLUS
#					PBM file
0	string		P1		image/x-portable-bitmap
#					PGM file
0	string		P2		image/x-portable-greymap
#					PPM file
0	string		P3		image/x-portable-pixmap
#					PBM "rawbits" file
0	string		P4		image/x-portable-bitmap
#					PGM "rawbits" file
0	string		P5		image/x-portable-greymap
#					PPM "rawbits" file
0	string		P6		image/x-portable-pixmap

# NIFF (Navy Interchange File Format, a modification of TIFF)
# [GRR:  this *must* go before TIFF]
0	string		IIN1		image/x-niff

# TIFF and friends
#					TIFF file, big-endian
0	string		MM		image/tiff
#					TIFF file, little-endian
0	string		II		image/tiff

# possible GIF replacements; none yet released!
# (Greg Roelofs, newt@uchicago.edu)
#
# GRR 950115:  this was mine ("Zip GIF"):
#					ZIF image (GIF+deflate alpha)
0	string		GIF94z		image/unknown
#
# GRR 950115:  this is Jeremy Wohl's Free Graphics Format (better):
#					FGF image (GIF+deflate beta)
0	string		FGF95a		image/unknown
#
# GRR 950115:  this is Thomas Boutell's Portable Bitmap Format proposal
# (best; not yet implemented):
#					PBF image (deflate compression)
0	string		PBF		image/unknown

# GIF
0	string		GIF		image/gif

# JPEG images
0	beshort		0xffd8		image/jpeg

# PC bitmaps (OS/2, Windoze BMP files)  (Greg Roelofs, newt@uchicago.edu)
0	string		BM		image/bmp
#>14	byte		12		(OS/2 1.x format)
#>14	byte		64		(OS/2 2.x format)
#>14	byte		40		(Windows 3.x format)
#0	string		IC		icon
#0	string		PI		pointer
#0	string		CI		color icon
#0	string		CP		color pointer
#0	string		BA		bitmap array


#------------------------------------------------------------------------------
# lisp:  file(1) magic for lisp programs
#
# various lisp types, from Daniel Quinlan (quinlan@yggdrasil.com)
0	string	;;			text/plain
# Emacs 18 - this is always correct, but not very magical.
0	string	\012(			application/x-elc
# Emacs 19
0	string	;ELC\023\000\000\000	application/x-elc

#------------------------------------------------------------------------------
# mail.news:  file(1) magic for mail and news
#
# There are tests to ascmagic.c to cope with mail and news.
0	string		Relay-Version: 	message/rfc822
0	string		#!\ rnews	message/rfc822
0	string		N#!\ rnews	message/rfc822
0	string		Forward\ to 	message/rfc822
0	string		Pipe\ to 	message/rfc822
0	string		Return-Path:	message/rfc822
0	string		Path:		message/news
0	string		Xref:		message/news
0	string		From:		message/rfc822
0	string		Article 	message/news
#------------------------------------------------------------------------------
# msword: file(1) magic for MS Word files
#
# Contributor claims:
# Reversed-engineered MS Word magic numbers
#

0	string		\376\067\0\043			application/msword
0	string		\320\317\021\340\241\261	application/msword
0	string		\333\245-\0\0\0			application/msword



#------------------------------------------------------------------------------
# printer:  file(1) magic for printer-formatted files
#

# PostScript
0	string		%!		application/postscript
0	string		\004%!		application/postscript

# Acrobat
# (due to clamen@cs.cmu.edu)
0	string		%PDF-		application/pdf

#------------------------------------------------------------------------------
# sc:  file(1) magic for "sc" spreadsheet
#
38	string		Spreadsheet	application/x-sc

#------------------------------------------------------------------------------
# tex:  file(1) magic for TeX files
#
# XXX - needs byte-endian stuff (big-endian and little-endian DVI?)
#
# From <conklin@talisman.kaleida.com>

# Although we may know the offset of certain text fields in TeX DVI
# and font files, we can't use them reliably because they are not
# zero terminated. [but we do anyway, christos]
0	string		\367\002	application/x-dvi
#0	string		\367\203	TeX generic font data
#0	string		\367\131	TeX packed font data
#0	string		\367\312	TeX virtual font data
#0	string		This\ is\ TeX,	TeX transcript text	
#0	string		This\ is\ METAFONT,	METAFONT transcript text

# There is no way to detect TeX Font Metric (*.tfm) files without
# breaking them apart and reading the data.  The following patterns
# match most *.tfm files generated by METAFONT or afm2tfm.
#2	string		\000\021	TeX font metric data
#2	string		\000\022	TeX font metric data
#>34	string		>\0		(%s)

# Texinfo and GNU Info, from Daniel Quinlan (quinlan@yggdrasil.com)
#0	string		\\input\ texinfo	Texinfo source text
#0	string		This\ is\ Info\ file	GNU Info text

# correct TeX magic for Linux (and maybe more)
# from Peter Tobias (tobias@server.et-inf.fho-emden.de)
#
0	leshort		0x02f7		application/x-dvi

# RTF - Rich Text Format
0	string		{\\rtf		application/rtf

#------------------------------------------------------------------------------
# animation:  file(1) magic for animation/movie formats
#
# animation formats, originally from vax@ccwf.cc.utexas.edu (VaX#n8)
#						MPEG file
0	string		\000\000\001\263	video/mpeg
#
# The contributor claims:
#   I couldn't find a real magic number for these, however, this
#   -appears- to work.  Note that it might catch other files, too,
#   so BE CAREFUL!
#
# Note that title and author appear in the two 20-byte chunks
# at decimal offsets 2 and 22, respectively, but they are XOR'ed with
# 255 (hex FF)! DL format SUCKS BIG ROCKS.
#
#						DL file version 1 , medium format (160x100, 4 images/screen)
0	byte		1			video/unknown
0	byte		2			video/unknown
#
# The following paramaters are created for Namazu.
# <http://openlab.ring.gr.jp/namazu/>
#
# 1999/06/15
0	string		<!--\ MHonArc		text/html; x-type=mhonarc
0	string		BZh			application/x-bzip2
