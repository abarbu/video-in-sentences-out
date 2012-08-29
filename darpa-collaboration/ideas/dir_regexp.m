function files = dir_regexp(directory, expression)

contents = dir(directory);

files = contents(arrayfun(@(file) not(isempty(regexp(file.name, expression))), contents));
