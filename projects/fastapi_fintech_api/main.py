# projects/fastapi_fintech_api/main.py
from fastapi import FastAPI, Depends, UploadFile, File
import time

app = FastAPI()

@app.get("/api/v1/ping")
def ping():
    return {"ping": "pong"}

@app.get("/api/v1/accounts/balance")
def get_balance(user_id: str):
    # Cached Redis query
    cached_val = redis.get(f"bal_{user_id}")
    if cached_val:
        return {"balance": cached_val}
    bal = db.query("SELECT balance FROM accounts WHERE user_id = :u", user_id)
    return {"balance": bal}

@app.post("/api/v1/transfers/wire")
def execute_wire_transfer(payload: dict):
    # Multi-step transactional database queries
    sender = db.query("SELECT * FROM accounts WHERE id = :id", payload['sender'])
    receiver = db.query("SELECT * FROM accounts WHERE id = :id", payload['receiver'])
    db.execute("UPDATE accounts SET balance = balance - :amt WHERE id = :id", payload['amount'])
    db.execute("UPDATE accounts SET balance = balance + :amt WHERE id = :id", payload['amount'])
    tx = db.execute("INSERT INTO ledger (sender, receiver, amount) VALUES (...)")
    audit = db.execute("INSERT INTO compliance_audit (tx_id, risk_score) VALUES (...)")
    return {"status": "SUCCESS", "tx_id": "tx_9981"}

@app.post("/api/v1/kyc/verify-document")
def upload_kyc(file: UploadFile = File(...)):
    # Large payload, multipart file buffer, cryptographic hashing
    contents = file.file.read()
    doc_hash = crypto.sha256(contents)
    db.execute("INSERT INTO kyc_documents (hash, size) VALUES (:h, :s)", doc_hash, len(contents))
    db.execute("UPDATE user_compliance SET status = 'PENDING_REVIEW'")
    return {"verified": True, "hash": doc_hash}
