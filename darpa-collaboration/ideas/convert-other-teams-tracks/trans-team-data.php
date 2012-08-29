#!/usr/bin/php
<?php

// Parse commandline arguments
if(count($argv) != 3) {
  error_log("Usage: ${argv[0]} team xml-filename");
  error_log("Aborting...");
  exit(1);
} else {
  $team = $argv[1];
  $filename = $argv[2];
}

// -------------------------------------------------------------------- Is the team valid?
$valid_teams = array('SRI', 'SBU', 'USC');
if(!in_array($team, $valid_teams)) {
  error_log("Invalide team '$team'. Valid values are: " . implode(", ", $valid_teams));
  error_log("Aborting...");
  exit(1);
}

// -------------------------------------------------------------------- Does the file exist?
if(!is_file($filename)) {
  error_log("Failed to find file: $filename, aborting");
  exit(1);
}

// -------------------------------------------------------------------- Process file by $team
if($team == 'SRI') {
  process_sri($filename);
} else if($team == 'SBU') {
  process_sbu($filename);
} else if($team == 'USC') {
  process_usc($filename);
} 

// -------------------------------------------------------------------- Utilities
function attr_map($elem) {
    $res = array();
    foreach($elem->attributes as $name => $node) {
      $res[$name] = $node->value;
    } 
    return $res;
}

function set_trackname($trackname) {
  $trackname = strtolower(trim($trackname));
  return $trackname == '' ? 'undefined' : ($trackname == 'human' ? 'person' : $trackname);
}

// -------------------------------------------------------------------- Process USC
function process_usc($filename)
{
  $team = 'USC';
  $path_parts = pathinfo($filename);
  $video_name = $path_parts['filename'];
  
  $suffix = '.groundtruth';
  if(substr($video_name, strlen($suffix) * -1) === $suffix) {
    $video_name = substr($video_name, 0, strlen($video_name) - strlen($suffix));
  }

  if($path_parts['extension'] != 'xml') {
    error_log("Does not look like a USC xml file: $filename");
    exit(1);
  }

  $xml_doc = new DOMDocument();
  if(!$xml_doc->load($filename)) {
    error_log("Failed to read dom document '$filename', aborting.");
    exit(1);
  }

  foreach($xml_doc->getElementsByTagName('Trajectory') as $elem) {
    $attrs = attr_map($elem);

    $track_id = (int) $attrs['obj_id'];
    $track_name = set_trackname($attrs['name']);
    if($track_name == '') exit(1);

    foreach($elem->getElementsByTagName('Frame') as $frame_elem) {
      $attrs = attr_map($frame_elem);

      $frame_no = (int) $attrs['frame_no'];
      $l = (int) $attrs['x'];
      $t = (int) $attrs['y'];
      $r = $l + (int) $attrs['width'];
      $b = $t + (int) $attrs['height'];
      
      echo "$team $video_name $track_id $frame_no $track_name $l $t $r $b\n";
    }
  }
}


// -------------------------------------------------------------------- Process SRI
function process_sri($filename)
{
  $team = 'SRI';
  $path_parts = pathinfo($filename);
  $video_name = $path_parts['filename'];

  if($path_parts['extension'] != 'xgtf') {
    error_log("Does not look like an SRI xml file: $filename");
    error_log("Aborting...");
    exit(1);
  }

  $xml_doc = new DOMDocument();
  if(!$xml_doc->load($filename)) {
    error_log("Failed to read dom document '$filename', aborting.");
    exit(1);
  }

  foreach($xml_doc->getElementsByTagName('object') as $elem) {
 
    $attrs = attr_map($elem);

    $track_id = isset($attrs['id']) ? (int) $attrs['id']: -1;
    $track_name = set_trackname($attrs['name']);
     
    // Now get the bbox elemens
    foreach($elem->getElementsByTagName('bbox') as $bbox_elem) {
      $attrs = attr_map($bbox_elem);
   
      $frame_span = explode(':', $attrs['framespan']);
 
      $l = (int) $attrs['x'];
      $t = (int) $attrs['y'];
      $r = $l + (int) $attrs['width'];
      $b = $t + (int) $attrs['height'];
    
      for($i = (int) $frame_span[0]; $i <= (int) $frame_span[1]; ++$i) {
	echo "$team $video_name $track_id $i $track_name $l $t $r $b\n";
      }
    }

  }
}

// -------------------------------------------------------------------- Process SBU
function process_sbu($raw_filename) {
  $team = 'SBU';
  
  $path_parts = pathinfo($raw_filename);
  $filename = $path_parts['filename'];
 
  // 'bbox_DROP4_A1_C1_Act2_PARK1_MC_MORN_906a2119-e83c-11df-b432-e80688cb869a.mov_1.csv';
  $matches = array();
  preg_match('/^bbox_([^\\.]+)\\.[^_]+_([0-9]+)$/', $filename, $matches);
  
  if(count($matches) == 0 || $path_parts['extension'] != 'csv') {
    error_log("Does not look like a SBU csv file: $filename");
    exit(1);
  }

  $video_name = $matches[1];
  $track_id = $matches[2];
  $track_name = "person";

  $contents = file_get_contents($raw_filename);
  $frame_no = 1;
  foreach(explode("\n", $contents) as $line) {
    // Skip blank lines
    if(trim($line) == '')  continue;

    $min_x = NAN;
    $max_x = NAN;
    $min_y = NAN;
    $max_y = NAN;

    $points = explode(",", $line);
    // We have sequences of 8 points -- 4 x-coords then 4 y-coords -- for body parts
    if(count($points) % 8 != 0) {
      error_log("Found a line in $filename which does not have mod-8 points per line: " . count($points));
      error_log($line);
      error_log("Aborting...");
      exit(1);
    }
    $idx = 0;
    while($idx < count($points)) {
      // Four x coords...
      for($i = 0; $i < 4; ++$i) {
        $min_x = min($min_x, $points[$idx]);
        $max_x = max($points[$idx], $max_x);
	$idx++;
      }
      // Four y coords...
      for($i = 0; $i < 4; ++$i) {
        $min_y = min($min_y, $points[$idx]);
        $max_y = max($points[$idx], $max_y);
	$idx++;
      }
    }
    
    echo "$team $video_name $track_id $frame_no $track_name $min_x $min_y $max_x $max_y\n";
    $frame_no++;
  }

}


