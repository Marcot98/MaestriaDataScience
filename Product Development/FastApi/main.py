from fastapi import FastAPI
from typing import Dict
app = FastAPI()

@app.get("/")
def root():
    return {"message": "Hello word, from Galileo Master!!! Section V"}

@app.get("/item/{item_id}")
def read_item(item_id: int) -> Dict[str, int]:
    return {"item_id": item_id}

