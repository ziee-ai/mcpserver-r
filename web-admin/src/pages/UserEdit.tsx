import { useEffect, useState } from "react";
import { Form, Input, Button, Card, Switch, Skeleton, App, Space } from "antd";
import { useNavigate, useParams } from "react-router-dom";
import { adminApi, User } from "../api/client";

export default function UserEdit() {
  const { id = "" } = useParams();
  const nav = useNavigate();
  const { message } = App.useApp();
  const [user, setUser] = useState<User | null>(null);
  const [form] = Form.useForm();

  useEffect(() => {
    (async () => {
      try {
        const u = await adminApi.getUser(id);
        setUser(u);
        form.setFieldsValue(u);
      } catch (e: any) {
        message.error(`Load failed: ${e?.message}`);
      }
    })();
  }, [id]);

  const onSubmit = async (vals: any) => {
    try {
      const updated = await adminApi.updateUser(id, {
        username: vals.username,
        email: vals.email || null,
        is_admin: vals.is_admin,
      });
      setUser(updated);
      message.success("Saved");
    } catch (e: any) {
      message.error(`Save failed: ${e?.response?.data?.message ?? e?.message}`);
    }
  };

  if (!user) return <Skeleton active />;
  return (
    <Card title={`Edit ${user.username}`}>
      <Form form={form} layout="vertical" onFinish={onSubmit}>
        <Form.Item
          label="Username"
          name="username"
          rules={[{ required: true }]}
        >
          <Input data-testid="edit-user-username" />
        </Form.Item>
        <Form.Item label="Email" name="email">
          <Input data-testid="edit-user-email" />
        </Form.Item>
        <Form.Item label="Admin" name="is_admin" valuePropName="checked">
          <Switch data-testid="edit-user-admin" />
        </Form.Item>
        <Form.Item>
          <Space>
            <Button type="primary" htmlType="submit" data-testid="edit-user-save">
              Save
            </Button>
            <Button onClick={() => nav("/users")}>Back</Button>
          </Space>
        </Form.Item>
      </Form>
    </Card>
  );
}
