#include <iostream>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include "errors.hpp"
#include "scanner.hpp"

using namespace holyc;

static void usageAndDie(){
	std::cerr << "Usage: holycc <infile>"
	<< " [-p]: Parse the input to check syntax\n"
	<< " [-t <tokensFile>]: Output tokens to <tokensFile>\n"
	<< " [-u <holycFile> <unparsedOutputFile>]: Parse the input file <holycFile> and pipe the unparsed tokens to <unparsedOutputFile>\n"
	;
	exit(1);
}

static void writeTokenStream(const char * inPath, const char * outPath){
	std::ifstream inStream(inPath);
	if (!inStream.good()){
		std::string msg = "Bad input stream";
		msg += inPath;
		throw new InternalError(msg.c_str());
	}
	if (outPath == nullptr){
		std::string msg = "No tokens output file given";
		throw new InternalError(msg.c_str());
	}

	Scanner scanner(&inStream);
	if (strcmp(outPath, "--") == 0){
		scanner.outputTokens(std::cout);
	} else {
		std::ofstream outStream(outPath);
		if (!outStream.good()){
			std::string msg = "Bad output file ";
			msg += outPath;
			throw new InternalError(msg.c_str());
		}
		scanner.outputTokens(outStream);
		outStream.close();
	}
}

static bool parse(const char * inFile, const char * unparseFile){
	std::ifstream inStream(inFile);
	if (!inStream.good()){
		std::string msg = "Bad input stream ";
		msg += inFile;
		throw new InternalError(msg.c_str());
	}
	std::ofstream* unparseOutStreamPtr = nullptr;
	if(unparseFile != nullptr) {
		unparseOutStreamPtr = new std::ofstream(unparseFile);
		if (!unparseOutStreamPtr->good()){
			std::string msg = "Bad input stream ";
			msg += unparseFile;
			throw new InternalError(msg.c_str());
		}
	}
	holyc::Scanner scanner(&inStream);
	holyc::Parser parser(scanner, unparseOutStreamPtr);
	int errCode = parser.parse();
	if(unparseOutStreamPtr != NULL) unparseOutStreamPtr->close();
	if (errCode != 0){ return false; }

	return true;
}

int 
main( const int argc, const char **argv )
{
	if (argc == 0){
		usageAndDie();
	}
	const char * inFile = NULL;
	const char * tokensFile = NULL;
	const char * unparseFile = NULL;
	bool checkParse = false;
	bool useful = false;
	bool writeUnparsed = false;
	int i = 1;
	for (int i = 1 ; i < argc ; i++){
		if (argv[i][0] == '-'){
			if (argv[i][1] == 't'){
				i++;
				tokensFile = argv[i];
				useful = true;
			} else if (argv[i][1] == 'p'){
				i++;
				checkParse = true;
				useful = true;
			} else if (argv[i][1] == 'u'){
				// The 'unparse' case
				// Take 2 input filenames: <input-file> <unparse-output-file>
				i++;
				checkParse = true;
				useful = true;
				writeUnparsed = true;
			} else {
			std::cerr << "Unrecognized argument: ";
			std::cerr << argv[i] << std::endl;
			usageAndDie();
			}
		} else {
			if (inFile == NULL){
				inFile = argv[i];
				if(writeUnparsed && unparseFile == NULL) {
					if(argc == 4) unparseFile = argv[3];
					else {
						std::cerr << "Provided invalid number of arguments for the -u flag\n";
						usageAndDie();
					}
				}
			} else {
				std::cerr << "Only 1 input file allowed with current options";
				std::cerr << argv[i] << std::endl;
				usageAndDie();
			}
		}
	}
	if (inFile == NULL){
		usageAndDie();
	}
	if (!useful){
		std::cerr << "Whoops, you didn't tell holycc what to do!\n";
		usageAndDie();
	}

	if (tokensFile != NULL){
		try {
			writeTokenStream(inFile, tokensFile);
		} catch (InternalError * e){
			std::cerr << "Error: " << e->msg() << std::endl;
		}
	}

	if (checkParse){
		try {
			bool parsed = parse(inFile, unparseFile);
			if (!parsed){
				std::cerr << "Parse failed" << std::endl;
			}
		} catch (ToDoError * e){
			std::cerr << "ToDo: " << e->msg() << std::endl;
			exit(1);
		}
	}
	
	return 0;
}
