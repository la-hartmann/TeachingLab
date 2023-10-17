import openai
from langchain.chains import RetrievalQA
from langchain.chat_models import ChatOpenAI
from langchain.document_loaders import PyPDFLoader
from langchain.document_loaders import UnstructuredPDFLoader
from langchain.embeddings.openai import OpenAIEmbeddings
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.vectorstores import Chroma
# Load your rubric from a JSON file
with open('data/student_work/mathematics_smarter_balanced_rubric.json', 'r') as file:
    rubric = json.load(file)

# Load the student's work from a PDF
student_work_loader = UnstructuredPDFLoader('data/student_work/test_math_file.pdf')
student_work_text = student_work_loader.load()  # This method might be different based on the actual library
student_work_text

# Check for any text that would indicate each of the different rubric points


# Get OPENAI Api key from R
openai.api_key = r.api_key_for_py

# 
