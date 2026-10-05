// projects/express_ecommerce_api/server.js
const express = require('express');
const app = express();

app.get('/health', (req, res) => {
  res.json({ status: 'ok' });
});

// projects/express_ecommerce_api/routes/auth.js
const router = express.Router();
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');

router.post('/api/v1/auth/login', async (req, res) => {
  const user = await db.users.findOne({ email: req.body.email });
  const valid = await bcrypt.compare(req.body.password, user.passwordHash);
  const token = jwt.sign({ id: user.id }, 'secret');
  res.json({ token });
});

// projects/express_ecommerce_api/routes/products.js
const redis = require('redis');

router.get('/api/v1/products', async (req, res) => {
  const cached = await redis.get('all_products');
  if (cached) return res.json(JSON.parse(cached));
  const products = await db.products.find({ active: true });
  await redis.set('all_products', JSON.stringify(products));
  res.json(products);
});

router.get('/api/v1/products/:id', async (req, res) => {
  const item = await db.products.findOne({ id: req.params.id });
  const reviews = await db.reviews.find({ productId: req.params.id });
  res.json({ item, reviews });
});

// projects/express_ecommerce_api/routes/checkout.js
router.post('/api/v1/checkout/process', async (req, res) => {
  // Heavy transactional checks
  const cart = await db.cart.findOne({ userId: req.body.userId });
  const inventory = await db.inventory.find({ items: cart.items });
  await db.orders.create({ orderId: '123', items: cart.items });
  await db.payment.create({ status: 'AUTHORIZED', amount: cart.total });
  await db.notifications.create({ type: 'EMAIL', userId: req.body.userId });
  await db.cart.delete({ userId: req.body.userId });
  res.json({ success: true, orderId: '123' });
});

// projects/express_ecommerce_api/routes/reports.js
router.get('/api/v1/analytics/export', async (req, res) => {
  const rawOrders = await db.orders.findMany({ dateRange: '30d' });
  const aggregated = await db.sales.query("SELECT SUM(amount) FROM sales GROUP BY store_id");
  const audit = await db.audit.query("SELECT * FROM audit_logs ORDER BY timestamp DESC LIMIT 500");
  const topUsers = await db.users.query("SELECT * FROM users WHERE active_spending > 1000");
  res.json({ aggregated, audit, topUsers });
});

module.exports = app;
