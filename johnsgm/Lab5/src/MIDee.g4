grammar MIDee;

@header {
package cas.cs4tb3.parser;

import cas.cs4tb3.MIDIHelper;
}

@members {
	private MIDIHelper midi = new MIDIHelper();
	double time;
	int currentInstrument = 0;
	int currentTempo = 120;
}

//Start rule
program
    :   (instrument
    	)* EOF 
    	{
    		midi.saveSequence();
    	}
    ;

instrument
	:	NAME (AT WHOLE)? '{' commands '}'
		{ 
			int tempId = midi.getInstrumentId($NAME.text);
			if (tempId != currentInstrument)
			{
				midi.setInstrument(tempId, 0);
				currentInstrument = tempId;
			}
			if ($WHOLE.text != null)
			{
				int tempTempo = Integer.parseInt($WHOLE.text);
				if (tempTempo != currentTempo)
				{
					midi.setTempo(tempTempo, 0);
					currentTempo = tempTempo;
				}
			}
		}
	;

commands
	:	(doPlay
		|doWait
		)*
	;

doPlay
returns [ArrayList<Integer> noteList]
	@init
	{
		$noteList = new ArrayList<Integer>();
	}
	:PLAY note {$noteList.add($note.id);} (SEPARATOR note {$noteList.add($note.id);})* FOR duration DELIM
		{
			for (Integer item : $noteList)
			{
				midi.play(item, midi.getDurationInTicks(time), midi.getDurationInTicks(time + $duration.val));
			}		
			time += $duration.val;
		}
	;

doWait
	:WAIT FOR duration DELIM
		{
			time += $duration.val;
		}
	;

note
returns[int id = 0]
	:NOTE_LETTER NOTE_MOD? NOTE_OCTAVE
		{
			$id = Integer.parseInt($NOTE_OCTAVE.text)*12;

			switch ($NOTE_LETTER.text){
				case "c": 	$id += 0;
							break;
				case "d": 	$id += 2;
							break;
				case "e": 	$id += 4;
							break;
				case "f": 	$id += 5;
							break;
				case "g": 	$id += 7;
							break;
				case "a": 	$id += 9;
							break;
				case "b": 	$id += 11;
							break;
				default : 	$id = 0;
							break;
			}
			if ($NOTE_MOD.text != null)
			{
				if ($NOTE_MOD.text == "#")
				{
					$id++;
				}
				else $id--;
			}
			if ($id > 127) $id = 127;
			else if ($id < 0) $id = 0;
		}
	;

duration
returns [double val = 0]
	:	(FLOAT {$val = Double.parseDouble($FLOAT.text);}
		|WHOLE {$val = Double.parseDouble($WHOLE.text);}
		|NOTE_OCTAVE {$val = Double.parseDouble($NOTE_OCTAVE.text);}
		)
	;

SPACE : [ \t\r\n]+ {skip();};
NOTE_LETTER : [a-g] ;
NOTE_MOD : ('#'|'_') ;
NOTE_OCTAVE : ([0-9]|'10') ;
WHOLE : [0-9]+ ;
FLOAT : [0-9]+ '.'? [0-9]* ;
PLAY : 'play' ;
WAIT : 'wait' ;
FOR : 'for' ;
DELIM : ';' ;
AT : '@' ;
SEPARATOR : ',' ;
NAME : [a-zA-Z]+ ;